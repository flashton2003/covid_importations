# covid_importations

The first stage in this analysis is to identify the introductions using matUtils introduce - [link]([url](https://usher-wiki.readthedocs.io/en/latest/tutorials.html#introduce-tutorial)).

Then, modify the `matUtils_introduce_analysis.Rmd` file, add a call to run_main() (e.g. line 123), which points to the introductions file output by matUtils introduce.

You also need to modify lines 46 and 47 to point to csvs from Our World In Data with the case numbers and restriction level information.
