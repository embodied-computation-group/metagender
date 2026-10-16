-- preprint.lua -- turn the clean manuscript into a two-column preprint set in
-- the HenriquesLab class (see preprint-template.tex).
--
-- Three jobs:
--   1. lift the front matter out of the body and hand it to the template as
--      metadata: title, authors with affiliation numbers, affiliations,
--      abstract, keywords, and the statement of relevance, which the template
--      sets as a headed block matching the abstract. The research transparency
--      statement moves to the end, before the acknowledgements.
--   2. float figures (figure* at full width for the landscape ones, 0.72 of
--      the text width otherwise) and tables (table* with the title above and
--      the legend below the tabular). longtable cannot live inside a
--      two-column float, so pandoc's longtable is rewritten as a tabular.
--   3. set the reference list with a hanging indent.
--
-- Gated on the `preprint` metadata flag; the other targets never see it. Run
-- AFTER changed.lua with clean=true, so the change marking is already gone.

local stringify = pandoc.utils.stringify

local active = false
local landscape = {}
local corresponding_author = nil

local function tex(blocks) return pandoc.write(pandoc.Pandoc(blocks), 'latex') end
local function itex(inlines) return tex({ pandoc.Plain(inlines) }) end
local function raw(s) return pandoc.RawBlock('latex', s) end
local function text(b) return stringify(b) end
local function is_header(b, level, pat)
  return b.t == 'Header' and b.level == level and text(b):match(pat) ~= nil
end

-- --------------------------------------------------------------------------
-- tables
-- --------------------------------------------------------------------------

local function is_table_title(b)
  return b.t == 'Para' and text(b):match('^Table%s+S?%d+%.') ~= nil
end

local function is_legend(b)
  return b.t == 'Div' and b.classes:includes('legend')
end

-- pandoc writes a longtable with p{} columns sized from --columns; in a float
-- the tabular has to size its own columns, so the colspec is replaced and the
-- longtable scaffolding removed.
local function tabular(tbl)
  local body = tex({ tbl })
  body = body:gsub('\\caption%b{}\\tabularnewline%s*', '')
  body = body:gsub('\\caption%[%]%b{}\\tabularnewline%s*', '')
  local ncols = #tbl.colspecs
  body = body:gsub('\\begin{longtable}%[%]%b{}',
                   '\\begin{tabular}{@{}' .. string.rep('l', ncols) .. '@{}}', 1)
  body = body:gsub('\\begin{longtable}%[%]', '\\begin{tabular}')
  body = body:gsub('\\end{longtable}', '\\end{tabular}')
  body = body:gsub('\\endfirsthead%s*', '')
  body = body:gsub('\\endhead%s*', '')
  body = body:gsub('\\bottomrule\\noalign{}%s*\\endlastfoot%s*', '')
  body = body:gsub('\\end{tabular}', '\\bottomrule\\noalign{}\n\\end{tabular}')
  body = body:gsub('\\begin{minipage}%[b%]{\\linewidth}\\raggedright%s*', '')
  body = body:gsub('%s*\\end{minipage}', '')
  return body
end

local function table_float(title, tbl, legend)
  local out = { '\\begin{table*}[t]', '\\centering', '\\footnotesize' }
  if title then out[#out + 1] = '\\caption*{' .. itex(title.content) .. '}' end
  out[#out + 1] = tabular(tbl)
  if legend then
    out[#out + 1] = '\\par\\vspace{5pt}\\begin{minipage}{\\textwidth}' ..
                    '\\footnotesize\\raggedright ' .. tex(legend.content) ..
                    '\\end{minipage}'
  end
  out[#out + 1] = '\\end{table*}'
  return raw(table.concat(out, '\n'))
end

-- --------------------------------------------------------------------------
-- figures
-- --------------------------------------------------------------------------

local function figure_float(fig)
  local path
  pandoc.walk_block(fig, { Image = function(im) path = path or im.src end })
  local base = path and path:match('([^/]+)$')
  local width = (base and landscape[base]) and '\\textwidth' or '0.72\\textwidth'
  local s = tex({ fig })
  s = s:gsub('\\includegraphics{', '\\includegraphics[width=' .. width .. ']{')
  s = s:gsub('\\pandocbounded{\\includegraphics%[([^%]]*)%]{', '\\includegraphics[width=' .. width .. ']{')
  -- captions already start "Figure N.", so the LaTeX number is suppressed
  s = s:gsub('\\caption{', '\\caption*{', 1)
  s = s:gsub('\\begin{figure}', '\\begin{figure*}[t]')
  s = s:gsub('\\end{figure}', '\\end{figure*}')
  return raw(s)
end

-- --------------------------------------------------------------------------
-- body
-- --------------------------------------------------------------------------

local function rebuild_body(blocks, transparency)
  local out = pandoc.List()
  local in_refs = false
  local i = 1
  while i <= #blocks do
    local b, nxt, nxt2 = blocks[i], blocks[i + 1], blocks[i + 2]
    if is_table_title(b) and nxt and nxt.t == 'Table' then
      local legend = (nxt2 and is_legend(nxt2)) and nxt2 or nil
      out:insert(table_float(b, nxt, legend))
      i = i + (legend and 3 or 2)
    elseif b.t == 'Table' then
      local legend = (nxt and is_legend(nxt)) and nxt or nil
      out:insert(table_float(nil, b, legend))
      i = i + (legend and 2 or 1)
    elseif b.t == 'Figure' then
      out:insert(figure_float(b))
      i = i + 1
    elseif is_header(b, 2, '^Acknowledgements') then
      -- the transparency statement goes here, and both become top-level sections
      if #transparency > 0 then
        out:insert(pandoc.Header(1, 'Research transparency statement'))
        out:extend(transparency)
      end
      out:insert(pandoc.Header(1, b.content))
      i = i + 1
    else
      if is_header(b, 1, '^References') then
        out:insert(b)
        out:insert(raw('\\begin{refslist}'))
        in_refs = true
      else
        out:insert(b)
      end
      i = i + 1
    end
  end
  if in_refs then out:insert(raw('\\end{refslist}')) end
  return out
end

-- --------------------------------------------------------------------------
-- front matter
-- --------------------------------------------------------------------------

-- "Kelly Hoogervorst^1^, Leah Banellis^1^, ..." -> {name, affil} pairs. The
-- corresponding author gets the letter symbol appended to their numbers.
local function parse_authors(inlines)
  local list, buf = pandoc.List(), pandoc.Inlines({})
  for _, el in ipairs(inlines) do
    if el.t == 'Superscript' then
      local name = stringify(buf):gsub('^[,%s]+', ''):gsub('[,%s]+$', '')
      local affil = pandoc.Inlines({ pandoc.Str(stringify(el.content)) })
      if corresponding_author and name == corresponding_author then
        affil:insert(pandoc.Str(','))
        affil:insert(pandoc.RawInline('latex', '\\Letter'))
      end
      list:insert(pandoc.MetaMap({ name = pandoc.MetaInlines(pandoc.Inlines({ pandoc.Str(name) })),
                                   affil = pandoc.MetaInlines(affil) }))
      buf = pandoc.Inlines({})
    else
      buf:insert(el)
    end
  end
  return pandoc.MetaList(list)
end

-- "| ^1^ Center of ..." line block -> {id, text} pairs
local function parse_affils(lb)
  local list = pandoc.List()
  for _, line in ipairs(lb.content) do
    local id, rest = nil, pandoc.Inlines({})
    for _, el in ipairs(line) do
      if not id and el.t == 'Superscript' then id = stringify(el.content)
      elseif id then rest:insert(el) end
    end
    while #rest > 0 and rest[1].t == 'Space' do rest:remove(1) end
    if id then
      list:insert(pandoc.MetaMap({ id = pandoc.MetaString(id), text = pandoc.MetaInlines(rest) }))
    end
  end
  return pandoc.MetaList(list)
end

local function keywords_string(para)
  local s = text(para):gsub('^Keywords:%s*', '')
  local parts = {}
  for k in s:gmatch('[^,]+') do parts[#parts + 1] = k:gsub('^%s+', ''):gsub('%s+$', '') end
  return table.concat(parts, ' | ')
end

function Meta(meta)
  active = meta.preprint ~= nil and stringify(meta.preprint) == 'true'
  if meta.corresponding_author then corresponding_author = stringify(meta.corresponding_author) end
  if meta.landscape_figures then
    for _, f in ipairs(meta.landscape_figures) do landscape[stringify(f)] = true end
  end
end

function Pandoc(doc)
  if not active then return nil end

  local title, authors, affils, keywords
  local abstract, relevance, transparency, rest =
    pandoc.List(), pandoc.List(), pandoc.List(), pandoc.List()
  local section = nil      -- which front-matter H2 we are inside

  for _, b in ipairs(doc.blocks) do
    if not title and b.t == 'Header' and b.level == 1 then
      title = b.content
    elseif title and not authors and b.t == 'Para' and not section then
      authors = b.content
    elseif not affils and b.t == 'LineBlock' then
      affils = b
    elseif b.t == 'Header' and b.level == 2 and
           (text(b):match('^Abstract') or text(b):match('^Statement of relevance')
            or text(b):match('^Research transparency statement')) then
      section = text(b)
    elseif b.t == 'Header' and b.level == 1 then
      section = nil
      rest:insert(b)
    elseif section and b.t == 'Para' and text(b):match('^Keywords:') then
      keywords = keywords_string(b)
    elseif section and section:match('^Abstract') then
      abstract:insert(b)
    elseif section and section:match('^Statement') then
      relevance:insert(b)
    elseif section and section:match('^Research') then
      transparency:insert(b)
    else
      rest:insert(b)
    end
  end
  if not title then return nil end

  local meta = doc.meta
  meta.title = pandoc.MetaInlines(title)
  if authors then meta.authors = parse_authors(authors) end
  if affils then meta.affiliations = parse_affils(affils) end
  meta.abstract = pandoc.MetaBlocks(abstract)
  if keywords then meta.keywords = pandoc.MetaString(keywords) end
  if #relevance > 0 then meta.relevance = pandoc.MetaBlocks(relevance) end

  local blocks = pandoc.List()
  blocks:extend(rebuild_body(rest, transparency))
  return pandoc.Pandoc(blocks, meta)
end

return { { Meta = Meta }, { Pandoc = Pandoc } }
