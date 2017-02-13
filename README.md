# Conway's Game of Life in Elm

I wrote this implementation of Conway's Life as a way to learn more about programming
in Elm. As such it has more than the usual number of comments, which hopefully will
be both helpful to other newbies and amusing to more experienced hands.

A live version of the app can be found here: http://www.madoverlord.com/Elm/Life/#

### Features List

* Single-step or free-run life generations at various speeds.
* Edit, shift, rotate, minimize, zoom, clear and "soup" the board.
* Import boards in .cell and .rle formats, export in .cell.
* Prestored patterns.
* Help (teal ? icon).
* Detects if running on a touch device and responds accordingly.
* Autoresizes.

### Setup

1. Run `npm install` to install the build process dependancies.
2. Run `npm install gulp -g` to install the gulp executable globally.
3. Run `elm package install` to install the elm dependancies.

### Start Build Process

1. Run `gulp` to start the build process.

### Running on your local machine

Load http://localhost:4000/ in your browser.

### Credit where Credit is Due

http://getbootstrap.com/ -- Documentation on Bootstrap.css
http://danielstern.ca/range.css/#/ -- Tool for customizing Bootstrap sliders
http://stackoverflow.com/questions/40269494/elm-adding-click-events-to-svg-elements-doesnt-work-is-this-possible -- insights on converting mouse locations from window-global to element-relative.
https://sourceforge.net/p/golly/discussion/467856/thread/2ea0e461/ -- optimal soup density

Thanks to @jessta and others on the Elm-slack #beginners channel for help.
