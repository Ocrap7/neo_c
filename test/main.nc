import std.io;

// This is public to other modules (files)
export int public_function() {
    500
}

export const int public_const = 40;

export struct PublicStruct {
    int value;
    int data;
}

struct Data {
    persist int instances = 0;

    int x;
    int y;

    Data create() {
        Data.instances++;
        Data {
            x: 0,
            y: 0
        }
    }

    // struct function
    int compute(self) {
        self.x + self.y
    }

    // static function
    bool get_something() {
        true
    }
}

enum OliverIdiosyncrasies{
    Potato,
    Hamud,
    OkOk,
    YeYeYe,
    ImNoBitchBitch
}

const int RANDOM_CONST = 500;

int main() {

    Data data { x: 0, y: 0 };
    Data more = Data.create();
    var example = Data.create();

    // Or should this be `let value`?
    var value = data.x;
    
    var enum_example = OliverIdiosyncrasies.Potato;
    // Get's the index of the enum (size is a type; this is a cast from the enum type to size)
    var enum_index = (size) enum_example

    if value == 0 {
        print("Value is zero");
    } else if value > 5 {
        print("Value larger than five");
    } else {
        print("Value is something else");
    }

    if value == (0 or 5 or 10 or 15) {
        print("Matches these values");
    }
    
    var array = [0, 1, 2, 3];
    int arr[5] = [0, 2, 4, 6, 8];
    var element = array[5];

    match enum_example {
        .Potato {
            print("Potato");
        }
        .Hamud {
            print("Hamud");
        }
        _ => {
            print("Other enum value");
        }
    }
    
    // reference (pointer)
    int &x = &data;

    var callback = int (int input) {
        input * 2
    }

    print("The value is: {}", value);

    // Iterator loop
    loop i in 0 to 10 {

    } 

    // While loop
    loop value < 10 {

    }

    // Infinite
    loop {

    }
    0 
}