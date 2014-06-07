var gulp = require('gulp')
  , purescript = require('gulp-purescript');

var paths = {
    src: [
        'src/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs'
    ],
    example: {
        app: {
            src: [
                'src/**/*.purs',
                'example/app/app.purs',
                'bower_components/purescript-*/src/**/*.purs'
            ]
        },
        tutorial: {
            src: [
                'src/**/*.purs',
                'example/tutorial/tutorial.purs',
                'bower_components/purescript-*/src/**/*.purs',
                'example/tutorial/bower_components/purescript-*/src/**/*.purs'
            ]
        },
        productTable: {
            src: [
                // 'src/**/*.purs',
                'example/thinking-in-react/producttable.purs',
                'bower_components/purescript-*/src/**/*.purs',
                'example/thinking-in-react/bower_components/purescript-*/src/**/*.purs'
            ]
        }
    }
};

var options = {
    example: {
        app: {
            output: 'example/app/app.js',
            main: true
        },
        tutorial: {
            output: 'example/tutorial/tutorial.js',
            main: 'Tutorial'
        },
        productTable: {
            output: 'example/thinking-in-react/producttable.js',
            main: 'ProductTable'
        }
    }
};

var compile = function(paths, options) {
    return function() {
        // We need this hack for now until gulp does something about
        // https://github.com/gulpjs/gulp/issues/71
        var psc = purescript.psc(options);
        psc.on('error', function(e) {
            console.error(e.message);
            psc.end();
        });
        return gulp.src(paths.src)
            .pipe(psc)
            .pipe(gulp.dest(paths.dest || 'js'));
    };
};

gulp.task('src', compile(paths, {}));

gulp.task('example-app', compile(paths.example.app, options.example.app));

gulp.task('example-tutorial',
    compile(paths.example.tutorial, options.example.tutorial));

gulp.task('example-productTable',
    compile(paths.example.productTable, options.example.productTable));

gulp.task('watch', function() {
    gulp.watch(paths.src, ['src']);
});

gulp.task('watch-examples', function() {
    var ps = paths.example.app.src.concat(paths.example.tutorial.src).concat(paths.example.productTable.src);
    gulp.watch(ps, ['example-app', 'example-tutorial', 'example-productTable']);
});

gulp.task('default', ['src', 'watch']);

gulp.task('examples', ['example-app', 'example-tutorial', 'example-productTable', 'watch-examples']);
