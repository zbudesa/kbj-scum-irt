# Functions
get_data <- function(url){
  tables <- url %>%
    read_html() %>%
    html_table()

  # Combine tables for Bond and Non-Bond
 data <- bind_rows(tables[[1]] %>%
              transmute(Title = `Epi­sode`,
                        across(2:5, as.double)),
            tables[[4]] %>% transmute(Title = `Season 2`,
                                      across(2:5, as.double))
  )
}

clean_data <- function(data){
  df <- data %>%
    # Removing the Raid because it's scores will fuck wit everything.
    filter(!grepl("Raid", Title),
           !grepl("Dr. No, without notes", Title)) %>%
    mutate(
      # Remove digits from titles
      Title = str_remove(Title, "[:digit:]+: "),
      # Add actor information
      Actor = c(
        # Bond 1-5
        rep("Sean Connery",5),
        # Bond 6
        "George Lazenby",
        # Bond 7
        "Sean Connery",
        # Bond 8-13
        rep("Roger Moore",6),
        # Bond 14
        "Sean Connery",
        # Bond 15
        "Roger Moore",
        # Bond 16 & 17
        rep("Timothy Dalton", 2),
        # Bond 18-21
        rep("Pierce Brosnan",4),
        # Original Casino Royale (1967)
        "Peter Sellers et al.",
        # Bond 24-30
        rep("Daniel Craig",5),
        # Bourne Ultimatum
        "Matt Damon",
        # Bourne Legacy
        "Jeremy Renner",
        # Bourne 5
        "Matt Damon",
        # Syriana
        "George Clooney, Matt Damon, Jeffrey Wright",
        # Breach
        "Chris Cooper, Ryan Phillippe, Laura Linney",
        # Enemy of the State
        "Will Smith, Gene Hackman",
        # Jack Ryan 1 & 2 + Air Force One
        rep("Harrison Ford", 3),
        # Jack Ryan 3
        "Ben Affleck",
        # Wild Chinese Movie
        "Jimmy Wang Yu",
        # Shadow Recruit
        "Chris Pine",
        # Another Tom Clancy
        "Michael B. Jordan",
        # Jennifer's Body
        "Megan Fox",
        # Man from uncle 1-3
        rep("Robert Vaughn",3),
        # Die Hard
        "Bruce Willis",
        # UNCLE 4-7
        rep("Robert Vaughn",5)
      ),
      series = c(
        rep("James Bond", 27),
        rep("Bourne", 3),
        rep("Other",3),
        rep("Jack Ryan(-ish)",4),
        "Other",
        rep("Jack Ryan(-ish)",2),
        "Other",
        rep("U.N.C.L.E.", 3),
        "Other",
        rep("U.N.C.L.E.", 5)
      ),
      year = c(1962,1963,1964,1965,1967,1969,1971,1973,1974,1977,
               1979,1981,1983,1983,1985,1987,1989,1995,1997,1999,
               2002,1967,2006,2008,2012,2015,2021,2007,2012,2016,
               2005,2007,1998,1992,1994,1997,2002,1967,2014,2021,
               2009,1964,1965,1966,1988,1966,1966,1967,1968,1968)) %>%
    rbind(

  # Having to add a bit more data by hand,
  # Double checking based on the podcast
  tibble(
    Title = c(
      "Return of the Man from UNCLE: Fifteen Years Later Affair",
      "The Man from U.N.C.L.E.","Atomic Blond"
    ),
    Smarm = c(
      4,7,7
    ),
    `Cul­tu­ral in­sen­si­ti­vity` = c(
      4,4,4
    ),
    `Un­pro­vo­ked vio­lence` = c(
      5,2,7
    ),
    `Mi­so­gy­ny` = c(
      4,6,7
    ),
    Actor = c(
      "Robert Vaughn","Henry Cavill","Charlize Theron"
    ),
    series = c(
      # Last two UNCLE moveis
      rep("U.N.C.L.E.", 2),"Other"
    ),
    year = c(
      1983,2015,2017
    )
  )
) %>%
    rename("cultural_insensitivity" = `Cul­tu­ral in­sen­si­ti­vity`,
           "unprovoked_violence" = `Un­pro­vo­ked vio­lence`,
           "misogyny" = `Mi­so­gy­ny`) %>%
    janitor::clean_names()
}

save_data <- function(df){
  write.csv(df, file = "data/scum_scores.csv")
}
