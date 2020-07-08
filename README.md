# GitHub-API

GitHub-API scrapes information about public repositories on GitHub via the API (see
https://developer.github.com/v3/), however it can be configured to make any standard GitHub API call. 

At the moment it currently scrapes the languages and respective line counts that make up a given repository. 
It also counts the contributors involved in a repository. In the example code the scraped information is used to get an understanding of popular languages
by deriving a total line count for each language across all repositories and also how many
contributors are involved with repositories using those languages. This allows us to get a snapshot
of the popular languages at a particular time by specifying whether to scrape older or more
recent repositories.

# 1. How to run
GitHub-API can be run by simply using “stack build” and “stack exec GitHub-API-exe”. In
order to change the repositories to be downloaded set the values of startRepoID and endRepoID
at the top of app/Main.hs by specifying the start and end repository IDs to scrape. 

Bear in mind the default API rate limit is 60/hour API requests, and each repository scraped currently results in an average of 2.01 total API calls. It is possible to include a GitHub personal access token in HTTP.hs. This increases the API limit from from 60/hour to 5000. Tokens can be created at https://github.com/settings/tokens. 

# 2. Overview
Stack is used to build the tool and manage the dependencies. See .yaml files in the root directory for configuration.

In the HTTP.hs module we use the Network.HTTP.Simple and Network.HTTP.Client.TLS libraries
to download public repository information in JSON format, returning a list of 100 repositories.
The Data.Aeson library is then used to parse this JSON into predefined data types (see
the Parser.hs and DataTypes.hs modules), which are then inserted into an SQLite3 database
using the Database.HDBC.Sqlite3 library (see DB.hs). In the HTTP module we then recursively
call the “languages_url” and “contributors_url” fields from the JSON for each repository. This
gives us the total line counts for each language that a given repository contains, as well as the
total number of contributors for that repository. This information is also stored in the database.
We then derive total line counts for each language we’ve seen and also build a total count of
contributors for each language i.e. if a repository has 3 languages and 10 contributors, we add 10
to the contributor count for that language overall. As another metric, we also derive an average
line count per contributor both overall per language and also averaged over the languages in
each repository.

Three API URLs are called:
 - http://api.github.com/repositories?since=repoID - RepoID is to be
   replaced with the repository ID and returns a page of 100
   repositories
 - https://api.github.com/repos/username/reponame/languages - Returns a
   list of languages and associated line counts for a given repository
 - https://api.github.com/repos/username/reponame/contributors - Returns
   a list of users who have contributed to a repository

An example of the raw JSON responses can be found in the directory “./example-API-calls”.

# 3. HTTP (src/HTTP.hs)
The HTTP module has two main requirements when making the API calls to GitHub. Firstly,
GitHub requires us to include a user agent header as part of the request, or the API call is
rejected. Additionally, we have to pass an authorisation token as a header, obtained from
https://github.com/settings/tokens to allow us to increase the API rate limit to 5000/hour. We
therefore have to insert these headers into each request made with callAPI.

When testing the program over thousands of API calls, we occasionally found that API calls
timed out or other HTTP exceptions were thrown, stopping program execution. To handle
these exceptions without stopping execution, the module includes error handling to print the HTTP
exception and continue to the next API call. To test this it's possible added calls during execution to a
website that can simulate HTTP response codes and timeouts (see httpstat.us and http://httpstat.us/504?sleep=60000).
