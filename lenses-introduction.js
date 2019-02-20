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

function fixDanielsLensTalk() {
    danielsLensTalk.creation.moment.date.day = 20;
}

function procrastinate(record) {
    record.creation.moment.date.day = record.creation.moment.date.day + 1;
}

// Impressions of JS approach to structs
//   1. Field access is good, though accessors are not first-class functions.
//   2. Field setting is super easy. It's ridiculous how easy it is.
//   3. Field modifying is slightly annoying, but it's not a huge deal.

// The JS model of structs uses in-place mutation. What if we wanted
// immutable copies? What would we need to provide?

function modifiedCopy(record, path, f) {
    var x = get(record, path);
    var y = f(x);
    return set(record, path, y);
}

// We'd need to provide:
//   1. A function to apply to the field
//   2. A struct
//   3. A 'path' into our struct, for use with a 'gets' function and
//      a 'sets' function (undefiend as of yet)

// Rearange the arguments and curry:
var modifies = (path) => (f) => (record) => {
    var x = gets(path)(record)
    var y = f(x)
    return sets(path)(y)(record);
};
// or more succinctly
var mods = (path) => (f) => (record) => sets( path )( f(gets(path)(record)) )( record );

// `mods` has type
//   `Path[struct, field] => Function[field, field] => Function[struct, struct]`
// `gets` has type
//   `Path[struct, field] => struct => field`
// `sets` has type
//   `Path[struct, field] => field => Function[struct, struct]`

// moreover, we'd love to be able to compose paths
//   `Path[outer, inner] => Path[inner, field] => Path[outer, field]`
