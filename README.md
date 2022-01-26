# CS565Grader

This is a tool for grading Coq homework for CS565, Purdue's graduate programming
language course. To use this tool, we also need to develop grading script (in
Coq, mostly Ltac) for each homework.

Currently it only supports BrightSpace (hard-coded), the learning platform
Purdue uses.

Unfortunately, the code has very few comments (my bad) at the moment.

## Workflow

This section outlines the workflow of using this tool for CS565. I assume you
have installed `stack` and know how to use it to build and run Haskell programs.
I will not talk about how to write grading scripts in this section, and I will
explain that in details in section (Grading scripts).
- Distribute homework: a homework typically consists of a homework file with
  exercises, a testing file for sanity check, a `Makefile` and `_CoqProject`
  file for building, and auxiliar files that the homework imports. Although the
  testing file is somewhat similar to the testing file used for grading, make
  sure to not reveal any answers in this testing file. See [an example
  homework](/example/for_student).

- Export grade book: go to BrightSpace, select the `Grades` tab, and click the
  `Export` button. Make sure we select the following options:
  + Key field: Org Defined ID
  + Grade values: points grade
  + User details: last name and first name
  
  Then select the homework we want to grade. Do not select more than one.
  Finally click `Export to CSV` to download a CSV file. We will call this
  `gradebook` file.
  
- Download submitted homework: go to BrightSpace, select `Course Tools` and then
  `Assignments`. Click the homework we want to grade. In the submission page, go
  to the buttom of this page and select `200 per pages`. Blame BrightSpace for
  not having a `download all` or `select all` button, and pray that you don't
  have more than 200 submissions. Now click the `select all rows` checkbox and
  then click `Download` button to download a zip file of all submissions.
  
- Grader file structure: See [an example grader directory](/example/for_grader).
  This directory, called `top` directory, consists of 3 directories: `input`,
  `output` and `build`. `input` directory contains students' homework, gradebook
  and our grading scripts. `output` directory will host the updated gradebook
  and feedback files for students after we finish grading. `build` directory is
  the working directory where we build and test homework.

- Create the `input` directory: copy the gradebook file to
  `input/gradebook.csv`. Unzip the submission zip file to a subdirectory called
  `hw`. This `hw` directory should have:
  + `index.html` for whatever reason
  + directory per submission: each directory should contain only one file, that
    is the homework file, usually called `hw<number>.v`. The directory name
    should have the format `<some id> - <last name> <first name> - <timestamp>`.
    There might be multiple submissions from the same student, and the grader
    will pick the latest one using the timestamp. However, BrightSpace
    constantly changes their timestamp format for no reason (they changed 3
    times in one semester!). In that case, the grader will throw an error
    (hopefully!), and you will have to update the parser for timestamp (`Lib.hs`
    -> `loadStudentDirs` -> `parseTime`).

  We also need to create a directory `aux`, consisting of grading scripts. I
  will talk about this in details later, but now you just need to know the
  grading scripts will produce a log file.
  
- Prepare the build directory: run `stack run -- prepare <top directory>
  <homework file>`. For example, `stack run -- prepare for_grader hw.v` if you
  try out this using the [example grader homework](/example/for_grader). Check
  the output of this command and see if there is anything weird, e.g., you may
  check if it indeed copies the latest submission if a student has multiple
  submissions. At the end of the output, it should say "All done!!".
  
  Also check the generated `build` directory (e.g., `for_grader/build`). It
  should consist of directories indexed by PUID, each of which contains the
  student's homework and the grading scripts from `aux`. You should try not to
  map these PUID to the students' names, to minimize bias.

- Build and grade homework: run `stack run -- grade <top directory>`. For
  example, `stack run -- grade for_grader`. It is possible that the compilation
  fails for some submissions (or even loops forever if they submitted bad proof
  scripts!). Go to that student's build directory and manually fix the issues.
  If it's their faults, edit the file `local.v` and modify `Comment` and
  `Penalty` to reflect that. More on that in section (Grading scripts).
  
- Manually grade some exercises: ideally the `grade` command finishes all the
  grading, but in most cases we need to manually grade some exercises. It is
  either because there are open questions, or because we need to give partial
  credits for some exercises. The grader allows us to write some automation
  (called sound verifier and sound falsifier) to decide if this student gets the
  answers correct, even if the questions might be open. Although a good
  automation can greatly reduce the number of exercises that require manual
  grading, we still need to manually grade some answers if the automation fail.
  
  After running the `grade` command, it should output a list of exercises and
  student IDs that requires manual grading (in the form of `<exercise name>:
  <student IDs>`). Manually process those exercises and update the scores in the
  corresponding `local.v`. You can also update the automation in testing scripts
  to handle this student's case. I will talk more about it in section (Grading
  scripts), but the key is to balance efforts spending on automation and efforts
  spending on manual grading. Strengthening the automation might take more work
  than manual grading if we overdo it. But it can save us a lot of efforts when
  it's done right.
  
  Re-run `grade` command until we finish all manual grading and it should output
  "All done!!". Usually I like to run `stack run -- grade <top directory> -f`
  one last time just to make sure nothing weird happens (`-f` will run `make
  clean` first).
  
- Generate `output` directory: run `stack run -- publish <top directory>`. The
  generated `output` directory should contain an updated `gradebook.csv` and a
  `feedback` directory which consists of `feedback.txt` for each student.
  
- Publish feedback and grades: go to `output/feedback`, and run `zip -r
  feedback.zip . -x ".*"` (on Mac, you may run `zip -r feedback.zip . -x ".*" -x
  "__MACOSX"`). This command is part of `Info-ZIP` that should be available in
  most UNIX-like systems by default (e.g., MacOS and Linux). DO NOT use other
  tools, whether or not they come with your system. I know it's easy to create a
  zip file from GUI, but it will most likely not work! Blame BrightSpace for
  that.
  
  Go to BrightSpace, and select `Grades` tab. Click `Import` button, and import
  the grades using `gradebook.csv` in `output` directory. Then go to `Course
  Tools` tab and click `Assignments`. After selecting the corresponding
  homework, click `Add Feedback Files` on top, and upload `feedback.zip`. You
  should check some students and see if `feedback.txt` appears as attachments.
  Finally, click the `select all rows` checkbox and then click `Publish
  Feedback`.
  
  Be careful that the homework category and homework name should not be the
  same! You can check that by going to `Grades` tab: the first line of the
  header is category and the second line is homework names. Importing grades or
  uploading feedback files could trigger yet another bug that prevents you from
  doing so if they use the same name. Again, blame BrightSpace.

- Inform the students: we have finished the grading! Announce it on whatever
  platform we decide to use (e.g., Piazza).

## Commands

TODO

## Grading scripts

TODO
