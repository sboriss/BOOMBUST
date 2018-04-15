


library( RPostgreSQL )

cat("\014")  # clear console
rm(list=ls(all=TRUE))

if( FALSE ){ 
  ### THIS WORKS ###
  cat("\014")  # clear console
  rm(list=ls(all=TRUE))    
  
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname="postgres", host="localhost", user="postgres", password="postgres", port="5432")
  
  #dbGetQuery(conn, "SET search_path to sboriss")
  #dbGetQuery(conn, "CREATE EXTENSION IF NOT EXISTS hstore;") 
  
  query_sql = "CREATE TABLE books ( id serial primary key, title VARCHAR (255), attr hstore );"
  dbGetQuery(conn, query_sql )
  
  query_sql = "INSERT INTO public.books (title, attr) VALUES ( 'PostgreSQL Tutorial', 'paperback => 243, publisher => postgresqltutorial.com,language => English, ISBN-13 => 978-1449370000, weight => 11.2ounces ' ) "
  dbGetQuery(conn, query_sql )  
  
  query_sql = "INSERT INTO public.books (title, attr) VALUES ( 'PostgreSQL Tutorial', 'paperback => 5, publisher => postgresqltutorial.com, language => English, ISBN-13 => 978-1449370001, weight => 1ounces ' ) "
  dbGetQuery(conn, query_sql )  
  
  
  query_sql = "SELECT svals (attr) FROM books WHERE id = 1;"
  dbGetQuery(conn, query_sql )
  
  query_sql = "SELECT attr -> 'weight' AS weight FROM books WHERE id = 1;"
  dbGetQuery(conn, query_sql )
  
  # close the connection
  dbDisconnect(conn)
  dbUnloadDriver(drv)
  
}


if( FALSE ){ 
  ### THIS DOES NOT WORK ###
  cat("\014")  # clear console
  rm(list=ls(all=TRUE))    
  
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname="sboriss", host="localhost", user="postgres", password="postgres", port="5432")
  
  #dbGetQuery(conn, "SET search_path to sboriss")
  dbGetQuery(conn, "CREATE EXTENSION IF NOT EXISTS hstore;") 
  
  query_sql = "CREATE TABLE public.books ( id serial primary key, title VARCHAR (255), attr hstore );"
  dbGetQuery(conn, query_sql )
  
  query_sql = "INSERT INTO public.books (title, attr) VALUES ( 'PostgreSQL Tutorial', 'paperback => 243, publisher => postgresqltutorial.com,language => English, ISBN-13 => 978-1449370000, weight => 11.2ounces ' ) "
  dbGetQuery(conn, query_sql )  
  
  query_sql = "INSERT INTO public.books (title, attr) VALUES ( 'PostgreSQL Tutorial', 'paperback => 5, publisher => postgresqltutorial.com, language => English, ISBN-13 => 978-1449370001, weight => 1ounces ' ) "
  dbGetQuery(conn, query_sql )  
  
  
  query_sql = "SELECT svals (attr) FROM books WHERE id = 1;"
  dbGetQuery(conn, query_sql )
  
  query_sql = "SELECT attr -> 'weight' AS weight FROM books WHERE id = 1;"
  dbGetQuery(conn, query_sql )
  
  # close the connection
  dbDisconnect(conn)
  dbUnloadDriver(drv)
  
}
if( FALSE ){ 
  ### THIS WORKS ###
  cat("\014")  # clear console
  rm(list=ls(all=TRUE))    
  
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname="postgres", host="localhost", user="postgres", password="postgres", port="5432")
  
  #dbGetQuery(conn, "SET search_path to sboriss")
  #dbGetQuery(conn, "CREATE EXTENSION IF NOT EXISTS hstore;") 
  
  query_sql = "CREATE TABLE ncstlvpseudo.books ( id serial primary key, title VARCHAR (255), attr hstore );"
  dbGetQuery(conn, query_sql )
  
  query_sql = "INSERT INTO ncstlvpseudo.books (title, attr) VALUES ( 'PostgreSQL Tutorial', 'paperback => 243, publisher => postgresqltutorial.com,language => English, ISBN-13 => 978-1449370000, weight => 11.2ounces ' ) "
  dbGetQuery(conn, query_sql )  
  
  query_sql = "INSERT INTO ncstlvpseudo.books (title, attr) VALUES ( 'PostgreSQL Tutorial', 'paperback => 5, publisher => postgresqltutorial.com, language => English, ISBN-13 => 978-1449370001, weight => 1ounces ' ) "
  dbGetQuery(conn, query_sql )  
  
  
  query_sql = "SELECT svals (attr) FROM ncstlvpseudo.books WHERE id = 1;"
  dbGetQuery(conn, query_sql )
  
  query_sql = "SELECT attr -> 'weight' AS weight FROM ncstlvpseudo.books WHERE id = 1;"
  dbGetQuery(conn, query_sql )
  
  # close the connection
  dbDisconnect(conn)
  dbUnloadDriver(drv)
  
}
