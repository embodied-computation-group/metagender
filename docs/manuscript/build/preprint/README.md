# Preprint class

`zHenriquesLab-StyleBioRxiv.cls` is the HenriquesLab bioRxiv template class, copied from https://github.com/HenriquesLab/HenriquesLab-biorxiv-template
(GPL-3.0, see LICENSE). The bioRxiv footer branding, the blue links and the bold
abstract are switched off in `../preprint-template.tex`, not by editing the class,
so a newer upstream copy can be dropped in. One local patch: the first of the two
`\RequirePackage{cleveref}` lines (before hyperref) is commented out, because current
cleveref refuses to be loaded before hyperref.
