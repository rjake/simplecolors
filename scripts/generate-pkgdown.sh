#!/bin/bash -e

#
# Generate a pkgdown site on a dedicated branch
# Based on: http://gscheibel.net/2016/05/15/Hosting-static-documentation-with-Asciidoctor-and-GH-pages.html
#

TARGET_BRANCH=gh-pages
DOCS_DIRECTORY=docs
COMMIT_MESSAGE='Update documentation'

# Delete gh-pages branch if it exists (useful for local testing)
$(git rev-parse --verify ${TARGET_BRANCH} &> /dev/null) && git branch -D ${TARGET_BRANCH}

# Create an orphaned branch
git checkout --orphan ${TARGET_BRANCH}

# Generate documentation
r -e 'pkgdown::build_site()'

# Add documentation (forcefully in case directory is ignored) and commit
git add -f --all "${DOCS_DIRECTORY}/"
git commit -am "${COMMIT_MESSAGE}"

# Force push the subtree
subtreeSha=$(git subtree split --prefix ${DOCS_DIRECTORY} ${TARGET_BRANCH})
git push origin ${subtreeSha}:refs/heads/${TARGET_BRANCH} --force