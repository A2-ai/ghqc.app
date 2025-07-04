# ghqc.app 0.7.1

- `ghqc_status_app()`: fixes small bug in which windows style line breaks aren't normalized for parsing the Relevant files section

# ghqc.app 0.7.0

- new feature that allows checklists to be imported via text files - this way checklist templates can be verbatim and take advantage of GitHub-flavored markdown features

# ghqc.app 0.6.5

- fixes bug in `ghqc_status_app()`in which the api url isn't included for the QC approved comment edit api call (works for github which is the default, but not GHE)

# ghqc.app 0.6.4

- fixes bug in `ghqc_notify_app()` in which not all ghqc Issues were listed in dropdown due to change in function filtering behavior now that the QC branch is also a label added to ghqc Issues.

# ghqc.app 0.6.3

- fixes bug created by ghqc.app 0.5.2 in which multiple ghqc Issues for a given QC file can be created in a Milestone (only one ghqc Issue for a given QC file per Milestone should be allowed)

# ghqc.app 0.6.2

- `ghqc_status_app()`: adds "remote commit ahead" to diagnostics in the case that the remote commit is ahead of the last posted qc commit even though the QC file hasn't changed.
- `ghqc_record_app()`: formatting fixes in summary tables

# ghqc.app 0.6.1

- fixes bug in which commits with quotes are incorrectly parsed

# ghqc.app 0.6.0

`ghqc_status_app()`:

- table buttons are now in an action drop-down menu
- action buttons exist to perform the actions:
    
    - Approve
    - Notify file changes
    - Notify latest commit
    - Repost last QC notification
    - Unapprove

# ghqc.app 0.5.3

- fixes bug in `ghqc_record_app()` in which in warning modal still notifies when there are no issues with unchecked checklist items

# ghqc.app 0.5.2

`ghqc_assign_app()`:

-   creates a label for created Issues including the QC branch
-   enforces restriction that Issues in a Milestone cannot span multiple
    QC branches
-   waiter when selecting files no longer takes up the entire screen, just the main panel

`ghqc_status_app()`:

-   retrieves open Milestones in Milestone drop-down by default

-   Milestone drop-down organizes Milestones by branch (applies to those
    created with `ghqc_assign_app()` in 0.5.2; legacy Issues won't be
    organized by branch, just in an unnamed group)

-   option to include closed Milestones in drop-down with a toggle

-   closed Milestones render in gray

-   default selected Milestones are now:

    -   the open Milestones on the user's branch (applies to those
        created with `ghqc_assign_app()` in 0.5.2)

    -   if no open Milestones on the user's branch, selects the most
        recently created open Milestone 
        
    - if no open Milestones, none are selected by default
    
-   global search removed

-   DT grammar bug for records info manually fixed ("1 records")

-   sidebar width minimized slightly

-   rename QC status "Requires approval" to "Closed without approval"

# ghqc.app 0.5.1

changes to `ghqc_record_app()`: - adds QC Status checks for Issues in
selected Milestones - summarizes QC Status checks in Milestone and Issue
summary tables - details more QC status information in Issue-level
sections

# ghqc.app 0.5.0

-   introduces `ghqc_status_app()`
-   renames `ghqc_resolve_app()` as `ghqc_notify_app()`
-   simplifies metadata in ghqc Issue bodies and comments
-   removes duplicate authors in collaborators list and removes emails
    configured automatically via git for clusters

# ghqc.app 0.4.18

-   fixes bug introduced in 0.4.17 in which assign app toggle bar didn't
    collapse

# ghqc.app 0.4.17

-   allows binary files to be assigned in `ghqc_assign_app()`
-   fixes bug in which file names with spaces aren't recognized

# ghqc.app 0.4.16

-   fixes bug in which tables don't hold position in QC Records
-   modifies wrapping in Milestone and Issue summary table columns
-   fixes bug in which "---" isn't escaped in markdown
-   fixes bug in which tables run off the page vertically

# ghqc.app 0.4.15

-   fixes bug in pre-app error checking in which an unset upstream
    branch is uncaught

# ghqc.app 0.4.14

changes to `ghqc_resolve_app()`:

-   changes button at the bottom of app from "Post Comment" to "Preview
    Comment" because the button itself doesn't post the comment
-   fixes glue parsing bugs in which variables don't exist in the case
    of errors
-   fixes commit log to shell out instead of calling `gert::git_log`
    which omits commits from merges
-   fixes commit log to only retrieve remote commits so that running the
    app on any local branch does not interfere with the log
-   fixes bug in which comments cannot be more than 65536 characters -
    for large file diffs, a link comparing the "previous" and "current"
    commits is provided for QCer reference

# ghqc.app 0.4.13

changes to `ghqc_record_app()`:

-   changes references to old terms "report" and "record" with new
    standardized term "QC Record"
-   copies rmd file and its sourced files to QC Record directory in case
    of error in app
-   fixes bullet point HTML syntax in app warning modal
-   fixes bug in which comment bodies with markdown syntax (like in-line
    code) didn't render in QC Record
-   fixes bug in which checklist headers weren't given their own line in
    QC Record rendering

# ghqc.app 0.4.12

-   fixes bug with rlang %\|\|% operator preventing the use of the
    Associate Relevant Files feature

# ghqc.app 0.4.11

-   allows metadata bullet parsing to recognize both `*` and `-`
    characters.

# ghqc.app 0.4.10

-   fixes error in Associate Relevant Files modal pop-up

# ghqc.app 0.4.9

-   fixes HTTP 404 error caused in attempts to download images from
    Issue comments
-   `ghqc_record_app()` returns in cases of errors instead of returning
    a modal pop-up
-   In `tryCatch`s, `e$message` is replaced with `conditionMessage(e)`

# ghqc.app 0.4.8

-   increases max number of commits in `gert::git_log()` to 9999
-   changes how current commit is calculated by using first element of
    `get_commits_df$commit` instead of first commit in git log from HEAD

# ghqc.app 0.4.7

-   fixes bug in `ghqc_resolve_app()` in which get_commits_df failed
    with error "argument of length 0" in the case of diverging branches
    not getting the entire git log.

# ghqc.app 0.4.6

-   fixes bug in `ghqc_record_app()` in which QC records were unable to
    be generated due to missing "qc type" from metadata in
    ghqc-generated issues since metadata simplification in ghqc 0.4.0
-   re-formats summary tables in QC Records given missing "qc type""
    column
-   removes `get_author` call from `ghqc_record` and replaces with
    author retrieved in Issue metadata
-   in Milestone summary table in QC Records, adds spaces between listed
    Issues in Milestones
-   removes author and collaborators from QC Data section in QC Records
    as this is redundant with Issue metadata

# ghqc.app 0.4.5

-   fixes bug in `ghqc_assign_app()` in which "No assigned QCer" isn't
    an option as soon as a QCer is selected for a QC file

# ghqc.app 0.4.4

-   edits UI in `ghqc_assign_app()` to bold selected qc file paths
-   edits UI in `ghqc_resolve_app()` in modal pop-up to clarify "Post
    Comment" and "Cancel" options
-   adds `shinyvalidate` marker to `ghqc_record_app()` to indicate
    required input of at least one milestone

# ghqc.app 0.4.3

-   fixes bug in which QCer is un-selectable if relevant files are
    selected for a QC file

# ghqc.app 0.4.2

-   fixes bug in which QCers dropdown caused error for selected files
    outside of the root directory

# ghqc.app 0.4.1

-   fixes bug in md5 hashes in resolve comment metadata to make hashes
    correct

# ghqc.app 0.4.0

-   fixes large logo bug
-   removes assignees section in left panel and isolates to right panel
-   changes language from "Assignee" to "QCer"
-   removes "Preview Comment" button from `ghqc_resolve_app()` and
    instead displays comment preview in modal pop-up upon selecting
    "Post Comment".
-   adds "Associate Relevant Files" section modal pop-up
-   simplifies metadata in ghqc Issues by removing file history and qc
    type
-   ghqc remote ssh compatibility implemented
-   stops apps on error using advice from this slack overflow:
    <https://stackoverflow.com/questions/70841568/why-do-i-have-to-terminate-r-in-between-every-shiny-app-run>

# ghqc.app 0.3.3

-   provides additional error handling in the resolve app in the case
    that the git branch isn't present in a given Issue's metadata
    section

# ghqc.app 0.3.2

-   fixes bug in resolve for Issues created in different branches that
    haven't been fetched locally

# ghqc.app 0.3.1

-   fixes a bug in parsing a glue component

# ghqc.app 0.3.0

-   Updates the custom configuration options repository (now
    `GHQC_OPTIONS_REPO`) check to reflect the following changes:
    -   The "note" file within the custom configuration repository is
        now `prepended_checklist_note` within "options.yaml"
    -   `checklist_display_name_var` in "options.yaml" provides option
        to change the name in which the QC checklists are referred to
        as.

# ghqc.app 0.2.0

Inclusive of 0.1.9, 0.1.10, 0.1.11 changes:

-   Adding the branch and a link to the file contents at the start of QC
    to the Metadata section of Issues created by the Assign app. Also
    added links to the file contents at the previous and current commits
    in the metadata section of the resolve app comments.
-   Adds "ghqc" label to all GitHub Issues created by ghqc. Only Issues
    with this label, and the Milestones containing them, will be
    viewable/selectable in the apps. This change is NOT backward
    compatible with Issues/Milestones created in previous versions
-   Clarifies language in Warning and Error modals for all three apps

# ghqc.app 0.1.11

-   Adding the branch and a link to the file contents at the start of QC
    to the Metadata section of Issues created by the Assign app. Also
    added links to the file contents at the previous and current commits
    in the metadata section of the resolve app comments.

# ghqc.app 0.1.10

-   Adds "ghqc" label to all GitHub Issues created by ghqc. Only Issues
    with this label, and the Milestones containing them, will be
    viewable/selectable in the apps. This change is NOT backward
    compatible with Issues/Milestones created in previous versions

# ghqc.app 0.1.9

-   Clarifies language in Warning and Error modals for all three apps

# ghqc.app 0.1.8

-   Removes reset button in record app modal for users to click after
    closing milestones when no closed milestones exist (was buggy in
    viewer panel)

# ghqc.app 0.1.7

-   Fixes bug in rendered reports in which a maximum height for logos
    wasn't set to prevent overlapping header lines and header text.

# ghqc.app 0.1.6

-   Fixes bug in record app for previewing markdown files (same bug and
    solution as in 0.1.4)

# ghqc.app 0.1.5

-   Fixes bug in assign and resolve apps - error checking did not catch
    remote changes to the directory because checks did not git fetch
    first

# ghqc.app 0.1.4

-   Fixes bug in resolve app for previewing markdown files

# ghqc.app 0.1.3

-   Changes "No Existing Milestones"" to "No Open Milestones" in
    ghqc_assign_app() Existing Milestones dropdown menu

# ghqc.app 0.1.2

-   Fixes bug in record app warning modal pop-up for unclosed
    milestones/issues/checklist items: Milestone link wasn't clickable
-   Improves "No closed milestones" modal pop-up warning in record app.
    Language change to "It is recommended to close milestones" and adds
    a reset link to modal for users to click after closing milestones.

# ghqc.app 0.1.1

-   In the case when someone sets the standard gh environment variable
    GITHUB_API_URL, each app checks if this URL matches the actual set
    remote URL. The function that gets the GITHUB_API_URL did not
    explicitly return a value.

-   Small grammar fix in error message from apps in the case that the
    checklists directory isn't in the cloned info repo.

# ghqc.app 0.0.0.9011

-   rename ghqc to ghqc.app and ghqc.launcher to ghqc

# ghqc 0.0.0.9010

-   terminology changed to match GitHub terminology like "Issue" and
    "Milestone"

-   git commit terminology changed from "reference" and "comparator" to
    "previous" and "current", respectively

-   current commit moved to top of metadata section

-   metadata sections moved to top of issue/comment bodies

-   "Create" app renamed to "Assign" app

-   "git sha" in metadata renamed to "initial qc commit" and moved to
    top of metadata section

# ghqc 0.0.0.9009

## New features

-   ghqc pulls client-specific information from a pre-existing repo
    using the environment variable `GIT_CLIENT_URL`, which is set equal
    to the https code link to the relevant github repo.

# ghqc 0.0.0.9008

## New features

-   ghqcLauncher is now ghqc.launcher to comply with standardized helper
    package naming conventions.

-   The report function is now a shiny app that can run in ghqc.launcher

-   The git credential authentication function is more robust in each
    case when

    1)  Git is already authenticated,
    2)  Git isn't already authenticated, and
    3)  Git is mis-authenticated.

-   Error handling for the git repo and Rproject is now more robust

-   The sidebar in `ghqc_create_app()` now scrolls for easy reading of
    long file and directory names

-   ghqc can function with multiple remotes set for a given repo, and
    the app selects the remote in the following hierarchy:

    1)  If a single remote exists, it selects it

    2)  Else, if multiple remotes exist:

    -   if the environment variable GHQC_REMOTE_NAME exists, it selects
        the one with that name

    -   else, if a remote named "origin" exists, it selects it

    -   else, it uses the first remote in the list of remotes

# ghqc 0.0.0.9007

## New features

-   Git credentials are set automatically when ghqc functions are run as
    long as the appropiate environment variables are set.

# ghqc 0.0.0.9006

## Minor improvements and bug fixes

-   Adds new toggle buttons to `ghqc_create_app()` for QC Item
    List/milestone so that user can either create a name for a new
    milestone or select a pre-existing milestone to add new QC items. If
    the new milestone name is a pre-existing one, functionality remains
    as before where the app adds items to the pre-existing milestone
    name.

-   Highlights/grays out existing files/issues in the file tree in
    `ghqc_create_app()` when selecting an existing milestone.

-   Adds more informative logging messages for git credential errors.

-   fixes bug in which the remote repo name was retrieved from the local
    repo name - as these names were previously always the same, the bug
    didn't arise until now.

# ghqc 0.0.0.9005

## Minor improvements and bug fixes

-   Fixes `treeNavigatorServer()` in `ghqc_create_app()` so that
    selecting directories with no viable children/files twice in a row
    does not cause file tree state invisible error.

-   Retrieves assignees from collaborators/members who have access to
    repo rather than entire list of members from an organization.

-   Fixes bug where `ghqc_create_app()` errors when there are no
    existing milestones.

-   Fixes bug where `ghqc_create_app()` errors when only one milestone
    exists.

-   Fixes bug where milestone is still created on GitHub even when
    process is aborted.

-   Fixes bug where code chunks in `ghqc_report()` overflowed the page
    if line was too long.

-   Updates checklists

# ghqc 0.0.0.9004

## New features

-   Installation and usage of the apps in the ghqc package now require
    the ghqcLauncher package, which allows the applications to be ran as
    background jobs.

-   Changes the available commits comparison in `ghqc_update_app()`
    from:

    1.  Initial QC commit and most recent QC issue update comment commit
    2.  Previous QC issue update comment commit and most recent QC issue
        update comment commit

    to:

    1.  Initial QC commit and most recent commit
    2.  Selectable "Reference" and "Comparator" commits (where
        Comparator is newer/more recent chronologically)

-   Adds a "preview" button for each selected QC file to allow users to
    preview the contents of the file in `ghqc_create_app()`.

-   Converts previous file tree from `shinyWidgets::treeInput()` to
    `jsTreeR::treeNavigatorServer()`/`jsTreeR::treeNavigatorUI()`.

    -   Loads only files that are from the opened directories rather
        than recursively getting the entire directory.

    -   Uses undetermined state on top level directories to prevent
        deselection unless all children are deselected.

    -   Filters out all binary files and returns a modalDialog that
        prevents further indexing into a directory if the directory only
        contains binary files and shows a list of the files. See
        `exclude_patterns()` and `list.files_and_dirs()` for full
        accounting of items that are excluded from the file tree.

-   ghqc_report() can take a vector milestones as its input, as well as
    an optional just_tables flag that will only output the tables in the
    report.

## Minor improvements and bug fixes

-   Adds additional status check to prevent issue creation in
    `ghqc_create_app()` if there is already an existing issue name of
    the selected file in the same milestone name.

-   Changes the checklist info button in `ghqc_create_app()` from a
    question mark symbol to text ("checklist info") to better show what
    it is for.

-   Adds "No Assignee" to dropdown selection for the individual file
    selection assignee and now defaults to it rather than first
    available assignee in `ghqc_create_app()`.

-   Moves all modalDialog (pop-ups) buttons to the top right for ease of
    closing without scrolling.

-   author in metadata is now the git user who published the most recent
    version of the script

-   file hashes for reference and comparator added to comment metadata

-   removes empty milestones in `get_open_milestone_objects()` and
    `get_open_milestone_object_names()`

-   `check_if_updates_since_init()` function

-   generate_qc_report() errors if any inputted milestones don't exist
    or are empty

-   in issue body metadata and report: author is the most recent
    modifier of a script on github and collaborators are other editors
    of the script (only appears if there are any collaborators)

-   in issue body metadata, file history url is now listed

-   in generate_qc_report(), Issue section renamed to QC data - file
    name removed (because that's the section name, and is thus
    redundant), milestone description is listed if it exists.

# ghqc 0.0.0.9003

## Minor improvements and bug fixes

-   Adds sorting by open/closed items to
    `ghqc_update_server()`/`ghqc_update_app()` for milestone specific
    issues.

-   Adds logging messages and timers to app initialization items and
    logging messages to gh api interactions.

-   Closes assignee dropdown box after selection in
    `ghqc_create_server()`/`ghqc_create_app()`.

-   fixed bug in milestone.R function milestone_exists

-   added checklists with subheaders to drop down in `ghqc_create_app()`

-   added link to github milestone in `ghqc_create_app()` success pop-up

-   added link to github issue in `ghqc_update_app()` success pop-up

-   improved summary table formatting in `generate_qc_report()`

-   fixed file difference bug in `ghqc_update_app()`

# ghqc 0.0.0.9002

## Minor improvements and bug fixes

-   Fixes `pmx_list()` so that it works on older versions of R.
