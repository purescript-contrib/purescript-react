var gulp = require('gulp')
  , gutil = require('gulp-util')
  , purescript = require('gulp-purescript')
  , webserver = require('gulp-webserver')
  ;

var paths = {
    src: [
        'src/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs'
    ],
    docs: 'docs/README.md',
    example: {
        app: {
            src: [
                'src/**/*.purs',
                'example/app/app.purs',
                'bower_components/purescript-*/src/**/*.purs'
            ]
        },
        tutorial: {
            dir: 'example/tutorial',
            src: [
                'src/**/*.purs',
                'example/tutorial/tutorial.purs',
                'bower_components/purescript-*/src/**/*.purs',
                'example/tutorial/bower_components/purescript-*/src/**/*.purs'
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
        }
    }
};

var compile = function(paths, options) {
    return function() {
        // We need this hack for now until gulp does something about
        // https://github.com/gulpjs/gulp/issues/71
        var psc = purescript.psc(options);
        psc.on('error', function(e) {
            gutil.log(e.message);
            psc.end();
        });
        return gulp.src(paths.src)
            .pipe(psc)
            .pipe(gulp.dest(paths.dest || 'js'));
    };
};

gulp.task('src', compile(paths, {}));

gulp.task('docs', function() {
    var docgen = purescript.docgen();
    docgen.on('error', function(e) {
        gutil.log(e.message);
        docgen.end();
    });
    return gulp.src('src/**/*.purs')
      .pipe(docgen)
      .pipe(gulp.dest(paths.docs))
});

gulp.task('example-app', compile(paths.example.app, options.example.app));

gulp.task('example-tutorial',
    compile(paths.example.tutorial, options.example.tutorial));

gulp.task('watch', function() {
    gulp.watch(paths.src, ['src']);
});

gulp.task('watch-example-app', function() {
    gulp.watch(paths.example.app.src, ['example-app']);
});

gulp.task('watch-example-tutorial', function() {
    gulp.watch(paths.example.tutorial.src, ['example-tutorial']);
});

gulp.task('run-example-tutorial', ['example-tutorial'], function() {
    gulp.src(paths.example.tutorial.dir)
        .pipe(webserver({
            livereload: true
        }));
});

gulp.task('default', ['src', 'watch']);

gulp.task('examples', ['example-app', 'example-tutorial']);
