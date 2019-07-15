var danielsLensTalk = {
    recordId: 0,
    creation: {
        user: 'danielbrice@gmail.com',
        moment: {
            date: {
                year: 2019,
                month: 5,
                day: 29
            },
            time: {
                hour: 19,
                minute: 0,
                second: 0
            }
        }
    },
    payload: 'Daniel\'s Lens Talk'
};


// I have this problem where I procrastinate.
function danielsLensTalkButLate() {
    danielsLensTalk.creation.moment.date.day = 30;
}


// I have this problem so frequently that I need help(er functions).
function procrastinate(record) {
    record.creation.moment.date.day = record.creation.moment.date.day + 1;
}


// Impressions of JS approach to structs:
//   1. Field access is nice, though accessors are not first-class.
//   2. Field setting is super easy. But it mutates the original.
//   3. Field modifying is slightly annoying (you repeat the path twice),
//      but it's not a huge problem. Again, it mutates the original.


// Let's do an immutable update by creating a new struct.
function procrastinate_immutable(record) {
    return {
        recordId: record.recordId,
        creation: {
            user: record.creation.user,
            moment: {
                date: {
                    year: record.creation.moment.date.year,
                    month: record.creation.moment.date.month,
                    day: record.creation.moment.date.day + 1
                },
                time: {
                    hour: record.creation.moment.time.hour,
                    minute: record.creation.moment.time.minute,
                    second: record.creation.moment.time.second
                }
            }
        },
        payload: record.payload
    };
}


// The above is verbose, brittle, and just ugly. It's not reusable:
// it does one (highly-specific) thing, and it does it poorly. It
// knows too much about things it shouldn't need to know anything about,
// making it hard to maintain and hard to reuse.


// We want reusable solutions. We want a bunch of little functions, each
// doing one general-purpose thing, and doing it well. Such little
// functions could be chained together to write out bigger functions.
// In other words, we want solutions that are **composable** and
// **polymorphic.**


// The JS model of structs uses in-place mutation. If we wanted to make
// a **first-class** representation of immutable updates--one that's
// composable and polymorphic--what information would we need to provide?


// My dream API: we have a value that represents a _path_ into a data
// structure, and we have some operations, _get_, _set_, and _mod_,
// that operate on paths. For example:
function mod(path, f, struct) {
    var x = get(path, struct);
    var y = f(x);
    return set(path, struct, y);
}


// Hypothetically:
//   * `get` has signature `(path, struct) => field`
//   * `set` has signature `(path, struct, field) => struct`
//   * `mod` has signature `(path, function, struct) => struct`


// Moreover, we'd love to be able to compose paths so that we
// could describe paths into nested fields:
//   `(path [outer -> inner], path [inner -> field]) => path [outer -> field]


// First we need to be able to make deep copies of javascript values.
// There are much better ways to make a deep copy of a javascript value.
// This method is brittle, don't use it in real life, but it gets the
// job done for the purposes of this demo.
function copy(struct) {
  return JSON.parse(JSON.stringify(struct));
}


// Now, how should we define a Path? The most straight-forward
// way is to define a Path to be just an object that has `get` and
// `set` methods that target a particular field in a struct. We can
// define `mod` and the cheekily-named `dot` in terms of `get` and `set`.
function Path(get, set) {
    return {
        get: get,
        set: set,
        mod: (struct, f) => {
            var x = get(struct);
            var y = f(x);
            return set(struct, y);
        },
        dot: (inner) => {
            return Path(
                // get(struct)
                (struct) => inner.get(get(struct)),
                // set(struct, x)
                (struct, x) => {
                    var old_outer = struct;
                    var old_inner = get(old_outer);
                    var new_inner = inner.set(old_inner, x);
                    var new_outer = set(struct, new_inner);
                    return new_outer;
                }
            );
        }
    };
}


// Now we have a (airquotes) class called `Path`, which has methods
// `get`, `set`, `mod`, and `dot`. The class constructor needs the
// user to provide the `get` and `set` methods.


// Conceptually, a Path is a first-class value that points to a
// particular (possibly-nested) field inside a larger data structure,
// providing methods for manipulating the data in that field in an
// immutable fashion.


// Using this API, we can define some Paths for our TODO app (or
// whatever it is).


// We define the `creation` Path. The intent is that this points into
// the `creation` field of our bespoke TODO app struct. But notice
// that this Path **knows nothing** about such details. This Path
// is polymorphic: it works for _any_ object that has a `creation`
// property.
var creation = Path(
    // define get
    (struct) => struct.creation,
    // define set
    (struct, x) => {
        var new_struct = copy(struct);
        new_struct.creation = x;
        return new_struct;
    }
);


// The `moment` Path is intended to point from the `creation` object
// into its `moment` property. But, again, it's polymorphic.
var moment = Path(
    // define get
    (struct) => struct.moment,
    // define set
    (struct, x) => {
        var new_struct = copy(struct);
        new_struct.moment = x;
        return new_struct;
    }
);


// The above two functions are _soo_ tedious. And they're _soo_ similar.
// We should be able to abstract out the part that varies as an argument.
// In fact, the only part that varies is the property name, so we should
// abstract that out as an argument.


// Think of `Field` as a subclass of `Path` that only needs you to
// provide the name of a property of an object.
function Field(field_name) {
    return Path(
        // define get
        (struct) => struct[field_name],
        // define set
        (struct, x) => {
            var new_struct = copy(struct);
            new_struct[field_name] = x;
            return new_struct;
        }
    );
}


// Here are a bunch of little functions, each doing one (general-purpose)
// thing well. (We don't need the old, clunky `creation` or `moment`
// anymore: we can redefine them in terms of `Field`. Fortunately, we
// made them mutable, so we can just overwrite them ;-) )
creation = Field('creation');
moment = Field('moment');
var date = Field('date');
var day = Field('day');


// Compare to `procrastinate`. Notice that here we only have to write
// out the path once XD
function procrastinate_lensy(record) {
    return creation.dot(moment).dot(date).dot(day).mod(record, (x) => x + 1);
}


// Load this file in your javascript REPL of choice, and try:
//
//     > danielsLensTalk.creation
//     > procrastinate_lensy(danielsLensTalk).creation
//     > danielsLensTalk.creation


// Out in the wild, what we've called a `Path` is usually called a `Lens`.


// A Lens is a first-class, composable value that describes a path into
// a (possibly-nested) field of a larger data structure, providing
// methods for accessing and manipulating the data located at that field
// of said records in a purely-immutable fashion.


// Thanks for reading! :-)
