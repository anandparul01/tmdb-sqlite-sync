

The purpose of the application is to fetch all the movies & TV show data from the TMDB database using APIs and stores them into a SQLite database.

## Contents
1. [Getting Started](#getting-started)</br>
2. [Application](#application)</br>
2.1. [Data](#data)</br>
2.2. [Software Stack](#software-stack)</br>
3. [Modules](#modules)</br>
3.1. [Database](#database)</br>
3.2. [Fetch](#fetch)</br>
3.3. [Parse](#parse)</br>
3.4. [Types](#types)</br>
4. [Authors](AUTHORS.md)</br>
5. [Change Log](CHANGELOG.md)</br>

## Getting Started
To create an SQLite database use `stack run -- create`</br>

To fetch data using APIs and persist it in database use `stack run -- loaddata`</br>

To generate and obtain a data.json file that contains all the data present in database use `stack run -- dumpdata`</br>

To query the database to fetch data use `stack run -- <query> <query_arguments>`</br>

## Application
**haskell-project v1.0.0.6**

### Data
[The Movie Database - TMDB](https://www.themoviedb.org/?language=en-GB)</br>
> [!NOTE]
> [Developer Docs](https://developer.themoviedb.org/docs/getting-started)

### Software Stack
<pre><a href="https://www.haskell.org"><img src="https://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg" width="30" height="30"/></a>  <a href="https://www.sqlite.org"><img src="https://upload.wikimedia.org/wikipedia/commons/9/97/Sqlite-square-icon.svg" width="30" height="30"/></a></pre>

## Modules
### Database
To interact with SQLite database
### Fetch
To fetch JSON data using APIs
### Parse
To parse the JSON received from APIs
### Types
To describe types used in the application
