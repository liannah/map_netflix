library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

movies <- readr::read_csv(here::here('data', 'processed/netflix_movies_genres.csv'))

app$layout(
    dbcContainer(
        list(
            htmlH1('NetViz - Netflix Movies Visualization Dashboard'),
            dccGraph(id='plot-area'),
            htmlDiv(id='output-area'),
            htmlBr(),
            htmlDiv(id='output-area2'),
            htmlBr(),
            dccDropdown(
                id='col-select',
                options = unique(movies$genre),
                value='Comedies', )
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('col-select', 'value')),
    function(type) {
        country_df <- movies %>%
            filter(genre == type) %>%
            dplyr::group_by(country) %>% 
            dplyr::count() %>% 
            dplyr::rename(count = n, name = country)
            
        country_code <- readr::read_csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") 
        country_code <- country_code %>% 
                dplyr::select(COUNTRY, CODE) %>% 
                dplyr::rename(name = COUNTRY, code = CODE)
        
        country_df <- merge(country_df, country_code, by="name")
        
        fig <- plotly::plot_ly(country_df, type='choropleth', locations=~code, z=~count, text=~name,
                               colorscale='Viridis', zmin = 0 , zmax=500,
                               marker=list(line=list(
                                   width=0)))
        
        fig <- fig %>% plotly::colorbar(title = "Movie Count")
        fig <- fig %>% plotly::layout(
            title = "World Map"
        )
        
        fig
    }
)

app$run_server(host = '0.0.0.0')
