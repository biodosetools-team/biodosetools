## Test environments

* Local installation (macOS BigSur 12.1): 4.1.2
* GitHub actions (ubuntu-latest): oldrel-1, release, devel
* GitHub actions (windows-latest): release
* Github actions (macOS-latest): release

## R CMD check results

0 errors | 0 warnings | 0 notes

There was 1 NOTE in Win-Builder:

> Found the following (possibly) invalid URLs:
>   URL: https://doi.org/10.1667/RR14145.1
>     From: NEWS.md
>     Status: 500
>     Message: Internal Server Error
>   URL: https://doi.org/10.1667/RR3198
>     From: NEWS.md
>     Status: 500

These URLs are confirmed working via `tools:::check_url_db()`, not sure what the issue is.
