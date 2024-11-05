FROM rocker/shiny:latest

# Install necessary system libraries
RUN apt-get update && apt-get install -y \
  curl \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libssh2-1-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  libicu-dev \
  liblzma-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* /tmp/downloaded_packages/ /tmp/*.rds /var/cache/apt/* /var/log/*

# Copy only dependencies file and install packages
WORKDIR /srv/shiny-server/
COPY ./app/dependencies.R /srv/shiny-server/

RUN Rscript dependencies.R

RUN rm -rf /srv/shiny-server/*


# # Copy application code and configuration
COPY ./app /srv/shiny-server/
# COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Make Shiny Server accessible at port 5000
EXPOSE 3838



# Change user to shiny
# USER shiny

CMD ["/usr/bin/shiny-server"]


# RUN rm -rf /srv/shiny-server/*

# WORKDIR /srv/shiny-server/

# COPY ./app /srv/shiny-server/

# # Copy configuration files into the Docker image
# COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# RUN Rscript dependencies.R

# # Make the ShinyApp available at port 5000
# EXPOSE 5000

# USER shiny

# CMD ["/usr/bin/shiny-server"]