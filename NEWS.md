# ghqc.app (development version)

# ghqc.app 0.0.0.9011

## Minor improvements

- rename ghqc to ghqc.app and ghqc.launcher to ghqc

# ghqc 0.0.0.9010

## Minor improvements

- terminology changed to match GitHub terminology like "Issue" and "Milestone"

- git commit terminology changed from "reference" and "comparator" to "previous" and "current", respectively

- current commit moved to top of metadata section

- metadata sections moved to top of issue/comment bodies

- "Create" app renamed to "Assign" app

- "git sha" in metadata renamed to "initial qc commit" and moved to top of metadata section

# ghqc 0.0.0.9009

## New features

- ghqc pulls client-specific information from a pre-existing repo using the environment variable `GIT_CLIENT_URL`, which is set equal to the https code link to the relevant github repo.

# ghqc 0.0.0.9008

## New features

- ghqcLauncher is now ghqc.launcher to comply with standardized helper package naming conventions.

- The report function is now a shiny app that can run in ghqc.launcher

- The git credential authentication function is more robust in each case when
    1) Git is already authenticated,
    2) Git isn't already authenticated, and
    3) Git is mis-authenticated.

- Error handling for the git repo and Rproject is now more robust

- The sidebar in `ghqc_create_app()` now scrolls for easy reading of long file and directory names

- ghqc can function with multiple remotes set for a given repo, and the app selects the remote in the following hierarchy:
   1) If a single remote exists, it selects it
  
   2) Else, if multiple remotes exist:
  
  - if the environment variable GHQC_REMOTE_NAME exists, it selects the one with that name

  - else, if a remote named "origin" exists, it selects it

  - else, it uses the first remote in the list of remotes

# ghqc 0.0.0.9007

## New features

-   Git credentials are set automatically when ghqc functions are run as long as the appropiate environment variables are set.

# ghqc 0.0.0.9006

## Minor improvements and bug fixes

-   Adds new toggle buttons to `ghqc_create_app()` for QC Item List/milestone so that user can either create a name for a new milestone or select a pre-existing milestone to add new QC items. If the new milestone name is a pre-existing one, functionality remains as before where the app adds items to the pre-existing milestone name.

-   Highlights/grays out existing files/issues in the file tree in `ghqc_create_app()` when selecting an existing milestone.

-   Adds more informative logging messages for git credential errors.

-   fixes bug in which the remote repo name was retrieved from the local repo name - as these names were previously always the same, the bug didn't arise until now.

# ghqc 0.0.0.9005

## Minor improvements and bug fixes

-   Fixes `treeNavigatorServer()` in `ghqc_create_app()` so that selecting directories with no viable children/files twice in a row does not cause file tree state invisible error.

-   Retrieves assignees from collaborators/members who have access to repo rather than entire list of members from an organization.

-   Fixes bug where `ghqc_create_app()` errors when there are no existing milestones.

-   Fixes bug where `ghqc_create_app()` errors when only one milestone exists.

-   Fixes bug where milestone is still created on GitHub even when process is aborted.

-   Fixes bug where code chunks in `ghqc_report()` overflowed the page if line was too long.

-   Updates checklists

# ghqc 0.0.0.9004

## New features

-   Installation and usage of the apps in the ghqc package now require the ghqcLauncher package, which allows the applications to be ran as background jobs.

-   Changes the available commits comparison in `ghqc_update_app()` from:

    1.  Initial QC commit and most recent QC issue update comment commit
    2.  Previous QC issue update comment commit and most recent QC issue update comment commit

    to:

    1.  Initial QC commit and most recent commit
    2.  Selectable "Reference" and "Comparator" commits (where Comparator is newer/more recent chronologically)

-   Adds a "preview" button for each selected QC file to allow users to preview the contents of the file in `ghqc_create_app()`.

-   Converts previous file tree from `shinyWidgets::treeInput()` to `jsTreeR::treeNavigatorServer()`/`jsTreeR::treeNavigatorUI()`.

    -   Loads only files that are from the opened directories rather than recursively getting the entire directory.

    -   Uses undetermined state on top level directories to prevent deselection unless all children are deselected.

    -   Filters out all binary files and returns a modalDialog that prevents further indexing into a directory if the directory only contains binary files and shows a list of the files. See `exclude_patterns()` and `list.files_and_dirs()` for full accounting of items that are excluded from the file tree.

-   ghqc_report() can take a vector milestones as its input, as well as an optional just_tables flag that will only output the tables in the report.

## Minor improvements and bug fixes

-   Adds additional status check to prevent issue creation in `ghqc_create_app()` if there is already an existing issue name of the selected file in the same milestone name.

-   Changes the checklist info button in `ghqc_create_app()` from a question mark symbol to text ("checklist info") to better show what it is for.

-   Adds "No Assignee" to dropdown selection for the individual file selection assignee and now defaults to it rather than first available assignee in `ghqc_create_app()`.

-   Moves all modalDialog (pop-ups) buttons to the top right for ease of closing without scrolling.

-   author in metadata is now the git user who published the most recent version of the script

-   file hashes for reference and comparator added to comment metadata

-   removes empty milestones in `get_open_milestone_objects()` and `get_open_milestone_object_names()`

-   `check_if_updates_since_init()` function

-   generate_qc_report() errors if any inputted milestones don't exist or are empty

-   in issue body metadata and report: author is the most recent modifier of a script on github and collaborators are other editors of the script (only appears if there are any collaborators)

-   in issue body metadata, file history url is now listed

-   in generate_qc_report(), Issue section renamed to QC data - file name removed (because that's the section name, and is thus redundant), milestone description is listed if it exists.

# ghqc 0.0.0.9003

## Minor improvements and bug fixes

-   Adds sorting by open/closed items to `ghqc_update_server()`/`ghqc_update_app()` for milestone specific issues.

-   Adds logging messages and timers to app initialization items and logging messages to gh api interactions.

-   Closes assignee dropdown box after selection in `ghqc_create_server()`/`ghqc_create_app()`.

-   fixed bug in milestone.R function milestone_exists

-   added checklists with subheaders to drop down in `ghqc_create_app()`

-   added link to github milestone in `ghqc_create_app()` success pop-up

-   added link to github issue in `ghqc_update_app()` success pop-up

-   improved summary table formatting in `generate_qc_report()`

-   fixed file difference bug in `ghqc_update_app()`

# ghqc 0.0.0.9002

## Minor improvements and bug fixes

-   Fixes `pmx_list()` so that it works on older versions of R.
