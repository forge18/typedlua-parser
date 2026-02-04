// Sample TypedLua code for benchmarking

// Basic variables
local x: number = 42;
local y: string = "hello";
local z: boolean = true;

// Functions
function add(a: number, b: number): number {
    return a + b;
}

function greet(name: string): string {
    return "Hello, " .. name;
}

// Classes
class Point {
    x: number;
    y: number;
    
    constructor(x: number, y: number) {
        this.x = x;
        this.y = y;
    }
    
    method distance(other: Point): number {
        return math.sqrt((this.x - other.x)^2 + (this.y - other.y)^2);
    }
}

// Interfaces
interface Shape {
    area(): number;
    perimeter(): number;
}

// Generic types
type Box<T> = {
    value: T;
};

// Complex expressions
local result = add(5, 3) * 2 + math.sin(0.5);
local message = greet("world");

// Template strings
local template = `The result is ${result}`;