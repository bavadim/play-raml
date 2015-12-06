# play-raml

play-raml is a sbt plugin for [play framework](https://www.playframework.com/) 2.4, 
which replaces the default routes definition syntax used in conf/routes with [RAML](http://raml.org). 
This plugin inspired by [sbt-play-raml](https://github.com/scalableminds/sbt-play-raml).

## Usage

- add the following lines to your sbt plugins file project/plugins.sbt:

        addSbtPlugin("bavadim" % "play-raml" % "0.1")
        
        resolvers += Resolver.url("bavadim", url("https://dl.bintray.com/bavadim/sbt-raml-plugins"))(Resolver.ivyStylePatterns)
        
- create your RAML routes file in conf/api.raml 
- add controller reference in first row of HTTP method description:

        /:
          get:
            description: |
              @controllers.Application.index