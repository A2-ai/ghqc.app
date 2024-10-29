
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ghqc.app <a href="https://github.com/a2-ai/ghqc.app/"><img src="man/figures/logo.png" align="right" height="139" alt="ghqc.app website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/ghqc.app/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/A2-ai/ghqc.app/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of the ghqc ecosystem is to simplify, standardize, and improve
traceability of the QC process through the use of shiny apps which
create GitHub Issues and Milestones for your project. ghqc.app is
intended to be used with the [ghqc](https://github.com/a2-ai/ghqc)
package, which will aid in the setup and launching of the shiny apps.
The ghqc.app package has 3 main applications/functions:

1.  `ghqc_assign_app`: This app helps the author assign a set of files,
    each with a corresponding checklist and QCer, to a GitHub Milestone
    to kick-off the QC review.

2.  `ghqc_resolve_app`: Throughout the QC review, this app helps the
    author communicate resolution of QC findings through comparison of
    file differences, hashes, and commits.

3.  `ghqc_record_app`: This app generates a record that compiles QC
    review that occurred on GitHub.

## Installation

You can install the development version of ghqc.app from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("a2-ai/ghqc.app")
```

## Interacting with ghqc

``` r
library(ghqc.app)
```

## Assign file(s) for QC

`ghqc_assign_app` provides an interface to assign one or more files for
QC in the form of a GitHub Issue(s) within a GitHub Milestone, with
options to assign a repository collaborator as the QCer and/or generate
a checklist of suggested review tasks during QC.

Each Issue created corresponds to a single file assigned to be reviewed
for QC. Issues are organized into Milestones as designated by the user.

#### To Assign QC

1)  Input a name to create a new Milestone or select an existing
    Milestone.

2)  Optional: if creating a new Milestone, input a description.

3)  Optional: select one or more collaborators who will be assigned to
    perform the QC. The selected collaborator(s) will not be assignee(s)
    until explicitly assigned to one or more selected files (Step 5
    below).

4)  Select one or more files from the file tree. Click the + signs to
    expand directories in the file tree.

5)  Optional: select an assignee for each selected file.

6)  Select a checklist type for each selected file.

7)  Post the Milestone by clicking “Assign File(s) for QC” on the bottom
    of the pane.

At any time, the user can:

- Click the `Preview file contents` button below a selected file to view
  its contents.

- Click the `Preview checklist` button below a selected file to view the
  items in a its selected checklist.

## Resolve QC finding(s)

`ghqc_resolve_app` allows a user to insert a comment into a ghqc GitHub
Issue that displays changes in the version control information for the
Issue’s corresponding file.

By default, the comment displays both the original and current commits
and hashes for the file. These versions are selected by the user. The
comment can optionally display the file difference (“diff”) between the
current and previous versions. These changes will likely be
implementations of QC feedback.

To use this app, first initialize one or more Issues with .

#### To comment in an Issue

1)  Optional: filter to the set of Issues within a Milestone.

2)  Select the Issue to be updated.

3)  Optional: provide a contextualizing message about the changes made
    to the file (e.g. “Implemented QC feedback for line 20”).

4)  Optional: insert the file difference display into the comment, by
    selecting “Show file difference”.  
    If displaying the file difference, choose to either:

    - compare the original version with the current version or,
    - compare a previous version with the current version.

5)  Optional: preview the comment before posting to the Issue.

6)  Post the comment to the Issue.

## Generate QC Record

`ghqc_record_app` allows the user to generate a QC Record for one or
more Milestones created with `ghqc_assign_app`.

#### To Generate a QC Record

1)  Select one or more Milestones.

    - optional to include both open and closed Milestones by unchecking
      “Closed Milestones only”.

2)  Optional: input a name for the PDF.

    - The default name is a hyphenated combination of the GitHub
      repository name and selected Milestone name(s).

3)  Optional: input the directory in which to generate the PDF.

    - The default directory is the root of the R project.

4)  Optional: indicate if the report should only include the Milestone
    and Issue summary tables by checking “Just tables”. Else, the
    default setting will generate a Record that contains the summary
    tables up front as well as detailed descriptions for each Issue
    including version control information, users, datetimes, events,
    actions, comments and more.

5)  Create the PDF by clicking “Generate QC Record” at the bottom of the
    pane.
