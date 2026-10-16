-- changed.lua -- render revision marks in blue in the PDF and Word output.
--
-- The section files mark revised text two ways:
--   inline:  [revised words]{.changed}
--   block:   ::: changed \n paragraphs \n :::
-- In LaTeX both become RevBlue, which header.tex defines. In docx both become
-- the `Revised` character style, which build/reference.docx defines in the same
-- blue. Every other output format is untouched, so the markdown still reads
-- cleanly on its own.
--
-- mdframed and changebar are not in this TeX installation, so blocks get a
-- colour group rather than a margin bar. A colour group also survives floats
-- and longtables inside a marked block, which the framed-style environments do
-- not. docx has no block equivalent: a paragraph custom-style on the div would
-- overwrite the Heading, Caption and table-cell styles of the blocks inside it,
-- so a marked div is coloured run by run instead.

-- A span or div with class `marknote` is a note *about* the marking ("Text in
-- blue was added..."). It is coloured with the rest in the review draft and
-- removed outright when the marking is off, together with the paragraph that
-- holds it.
--
-- Either the `preprint` or the `clean` metadata flag turns the colour off: both
-- targets keep the revised text but show no review marking. The markdown is
-- unchanged either way, so every target reads from the same section files.
local marking = true

local function is_latex()
  return FORMAT:match('latex') ~= nil
end

local function is_docx()
  return FORMAT == 'docx'
end

-- True for the elements this filter acts on at all; `note` says which kind.
local function marked(el)
  local note = el.classes:includes('marknote')
  return (note or el.classes:includes('changed')), note
end

local function mark(el, open, close, wrap, empty)
  local hit, note = marked(el)
  if not (is_latex() and hit) then return nil end
  if not marking then return note and empty or el.content end
  local out = pandoc.List({ wrap('latex', open) })
  out:extend(el.content)
  out:insert(wrap('latex', close))
  return out
end

-- docx: a run of inlines in the Revised character style.
local function revised(inlines)
  return pandoc.Span(inlines, pandoc.Attr('', {}, { ['custom-style'] = 'Revised' }))
end

local function flag(meta, key)
  return meta[key] ~= nil and pandoc.utils.stringify(meta[key]) == 'true'
end

local function get_meta(meta)
  marking = not (flag(meta, 'preprint') or flag(meta, 'clean'))
end

local function Span(el)
  if is_docx() then
    local hit, note = marked(el)
    if not hit then return nil end
    if not marking then return note and {} or el.content end
    return revised(el.content)
  end
  return mark(el, '\\textcolor{RevBlue}{', '}', pandoc.RawInline, {})
end

local function Div(el)
  if is_docx() then
    local hit, note = marked(el)
    if not hit then return nil end
    if not marking then return note and {} or el.content end
    -- Style the text of every paragraph and heading in the block, leaving each
    -- block's own paragraph style (Heading, Caption, table cell) in place.
    return pandoc.walk_block(el, {
      Para   = function(b) return pandoc.Para({ revised(b.content) }) end,
      Plain  = function(b) return pandoc.Plain({ revised(b.content) }) end,
      Header = function(b)
        return pandoc.Header(b.level, { revised(b.content) }, b.attr)
      end,
    }).content
  end
  return mark(el, '\\begin{revchanged}', '\\end{revchanged}', pandoc.RawBlock, {})
end

-- A dropped marknote span leaves its paragraph behind empty; take that with it.
-- Inlines are visited before the block that holds them, so by here the span is
-- already gone.
local function Para(el)
  if not marking and #el.content == 0 then return {} end
end

-- Metadata has to be read before the spans and divs are visited.
return {
  { Meta = get_meta },
  { Span = Span, Div = Div, Para = Para },
}
