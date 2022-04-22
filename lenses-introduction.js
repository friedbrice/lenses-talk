// I was going to give my lens talk on 19 Feb...
var danielsLensTalk = {
    recordId: 0,
    creation: {
        user: 'danielbrice@gmail.com',
        moment: {
            date: {
                year: 2019,
                month: 2,
                day: 19
            },
            time: {
                hour: 18,
                minute: 30,
                second: 0
            }
        }
    },
    payload: 'Daniel\'s Lens Talk'
};

// ...buuuut, I was running a little behind.
// That's easy-enough to fix.
function fixDanielsLensTalk() {
    danielsLensTalk.creation.moment.date.day = 20;
}

// But what if we want to abstract that pattern?
// (Just as, like, an exercise, ya know? Nothing to do with getting cold feet!)
function procrastinate(record) {
    record.creation.moment.date.day = record.creation.moment.date.day + 1;
}

// Impressions of JS approach to nested data structures
//   1. Field access is good, though accessors are not first-class functions.
//   2. Field setting is super easy. It's ridiculous how easy it is.
//   3. Field modifying is slightly annoying, because of the duplication, but it's not a huge deal.

// The JS model relies on in-place mutation.
// What if we wanted immutable copies, instead?
// What data would we need to provide to do it?

function modifiedCopy(record, path, f) {
    var x = get(record, path);
    var y = f(x);
    return set(record, path, y);
}

// We'd need to provide:
//   1. A function to apply to the field (`f` in the example above)
//   2. A struct (`record` in the example above)
//   3. A `path` referencing a piece of our data structure, for use with a generic `get` function and a generic `set` function (so far undefiend).

// Rearange the arguments and curry (and give them cooler names):
var modifies = (path) => (f) => (record) => {
    var x = gets(path)(record)
    var y = f(x)
    return sets(path)(y)(record);
};
// or more succinctly
var mods = (path) => (f) => (record) => sets( path )( f(gets(path)(record)) )( record );

// `gets` has this type:
//
//     gets: (Path<struct, field>) => Function<struct, field>
//
// So `gets` takes a path and gives you back an accessor function.
//
//
// `sets` has type
//
//     sets: (Path<struct, field>) => (field) => Function<struct, struct>
//
// So `sets` takes a path and gives you back a setter function.
//
//
// And `mods` has this type:
//
//     mods: (Path<struct, field>) => (Function<field, field>) => Function<struct, struct>
//
// So `mods` takes a path and gives you back a function that will promote a transformation of the field data to a transformation of the overall struct.


// Last but not least, we'd love to be able to trace along two paths in sequence (so long as one ends where the other one starts).
// And we'd like to be able to think of the result as a single path on its own.
//
//     trace: (Path<outer, middle>) => (Path<middle, inner>) => Path<outer, inner>
//
