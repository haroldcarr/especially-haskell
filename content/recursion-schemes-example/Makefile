#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2015 Dec 14 (Mon) 19:16:54 by Harold Carr.
#

G                   = .generated
INFRA               = ../../infrastructure/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin/infra

all : $(G)/PRS.hs.org $(G)/PRS.html

# hs_to_org
$(G)/PRS.hs.org : src/PRS.hs
	$(INFRA) $< $@

# TODO why is hcEmacsOrgModeExportHtml output relative to input rather than absolute?
# org_to_html
$(G)/PRS.html : src/PRS.org
	hcEmacsOrgModeExportHtml $< ../$(G)/PRS.html

clean :
	stack clean
	rm $(G)/*

# End of file.
