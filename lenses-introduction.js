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

function fixDanielsLensTalk() {
    danielsLensTalk.creation.moment.date.day = 30;
}

function procrastinate(record) {
    record.creation.moment.date.day = record.creation.moment.date.day + 1;
}

// Impressions of JS approach to structs
//   1. Field access is nice, though accessors are not first-class functions.
//   2. Field setting is super easy. But it mutates the original.
//   3. Field modifying is slightly annoying, but it's not a huge problem.

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

// The JS model of structs uses in-place mutation. What if we wanted
// immutable copies, compositionally and polymorphically? What would we
// need to provide?

function mod(path, f, struct) {
    var x = get(path, struct);
    var y = f(x);
    return set(path, struct, y);
}

// We'd need to define some functions:
//   1. A function to apply to the field
//   2. A struct
//   3. A 'path' into our struct, for use with a 'get' function and
//      a 'set' function (undefiend as of yet)

// `mod` has type
//   `(path, function, struct) => struct`
// `get` has type
//   `(path, struct) => field`
// `set` has type
//   `(path, struct, field) => struct`

// moreover, we'd love to be able to compose paths
//   `(path [outer->inner], path [inner->field]) => path [outer->field]

function copy(struct) {
  return JSON.parse(JSON.stringify(struct));
}

// Easiest thing to do is make a `Path` just an object that defines
// `get` and `set` methods that target a particular field in a struct.
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
                struct => inner.get(get(struct)),
                // set(struct, x)
                (struct, x) => set(struct, inner.set(get(struct), x))
            );
        }
    };
}

var creation = Path(
    // get
    (struct) => struct.creation,
    // set
    (struct, x) => {
        var new_struct = copy(struct);
        new_struct.creation = x;
        return new_struct;
    }
);

var moment = Path(
    // get
    (struct) => struct.moment,
    // set
    (struct, x) => {
        var new_struct = copy(struct);
        new_struct.moment = x;
        return new_struct;
    }
);

function Field(field_name) {
    return Path(
        // get
        (struct) => struct[field_name],
        // set
        (struct, x) => {
            var new_struct = copy(struct);
            new_struct[field_name] = x;
            return new_struct;
        }
    );
}

var creation = Field('creation');
var moment = Field('moment');
var date = Field('date');
var day = Field('day');

function procrastinate_lensy(record) {
    return creation.dot(moment).dot(date).dot(day).mod(record, (x) => x + 1);
}
