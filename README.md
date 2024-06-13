# Nuolja Transect

## Instructions
1. Data directories are put into the `data` directory.
2. Run `Rscript script.r` to generate files into the `/repack` directory and generate output files into the `/out` directory.
3. Options will be prompted to help validate or generate files.

## Notes about data
There is inconsistent date naming and one entry from 2022 is missing a valid date.

## Documentation
The full documentation for this project is available as a GitHub Pages site. You can access the documentation [here](https://your-github-username.github.io/your-repo-name).

This documentation includes detailed information about the project's structure, data processing steps, and usage examples. It is generated automatically from the source code comments using `roxygen2` and `pkgdown`.

### How to Update Documentation
1. Make sure your code is properly documented using `roxygen2` comments.
2. Push your changes to the `beta` branch.
3. The GitHub Actions workflow will automatically generate and deploy the updated documentation to GitHub Pages.

### Example
To see an example of how to document your functions, check the comments in `script.r`:

```r
#' Title of your function
#'
#' Detailed description of your function.
#'
#' @param x Description of parameter x.
#' @param y Description of parameter y.
#' @return Description of the return value.
#' @export
#' @examples
#' example_function(1, 2)
example_function <- function(x, y) {
  # Your code here
  
}
```
