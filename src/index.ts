export type EnumToObject<T extends object> = {
    [K in keyof T as T[K] extends number ? K : never]: T[K];
};


export type EnumToMapNames<T extends object> = {
    [K in keyof T as T[K] extends number ? T[K] : never]: K;
};


export type ValueOf<T> = T[keyof T];


export type ReverseMap<T> = {
    [K in keyof T as T[K] extends number | string | symbol ? T[K] : never]: K;
}


/**
 *
 * Utility type to force TypeScript to show the expanded object
 * `Pick<T, "key" | "key2">` to `{ key: ..., key2: ... }`
 */
export type Expand<T> = T extends infer U ? { [K in keyof U]: U[K] } : never;


/**
 * expand `keyof Pick<T, "key"> | Pick<T, "key2">` to `"key" | "key2"`
 */
export type KeysOfUnion<T> = T extends T ? keyof T : never;


/**
 * convert `{ foo: string } | {  bar: string }` to ` { foo: string } & { bar: string }`
 */
export type UnionToIntersection<U> =
    (U extends unknown ? (k: U) => void : never) extends (k: infer I) => void
        ? I
        : never;



// Helper type. Not useful on its own.
type Without<FirstType, SecondType> = {[KeyType in Exclude<keyof FirstType, keyof SecondType>]?: never};


/**
Create a type that has mutually exclusive keys.

This type was inspired by [this comment](https://github.com/Microsoft/TypeScript/issues/14094#issuecomment-373782604).

This type works with a helper type, called `Without`. `Without<FirstType, SecondType>` produces a type that has only keys from `FirstType` which are not present on `SecondType` and sets the value type for these keys to `never`. This helper type is then used in `MergeExclusive` to remove keys from either `FirstType` or `SecondType`.

@example
```
import type {MergeExclusive} from '@cites/utility-types';

interface ExclusiveVariation1 {
    exclusive1: boolean;
}

interface ExclusiveVariation2 {
    exclusive2: string;
}

type ExclusiveOptions = MergeExclusive<ExclusiveVariation1, ExclusiveVariation2>;

let exclusiveOptions: ExclusiveOptions;

exclusiveOptions = {exclusive1: true};
//=> Works
exclusiveOptions = {exclusive2: 'hi'};
//=> Works
exclusiveOptions = {exclusive1: true, exclusive2: 'hi'};
//=> Error
```

@category Object
*/
export type MergeExclusive<FirstType, SecondType> =
    (FirstType | SecondType) extends object ?
        (Without<FirstType, SecondType> & SecondType) | (Without<SecondType, FirstType> & FirstType) :
        FirstType | SecondType;


export type KnownKeys<T> = keyof {
    [K in keyof T as string extends K ? never : number extends K ? never : K]: unknown
};

// --- A helper to decrement numbers for recursion depth limiting ---
type Prev = [never, 0, 1, 2, 3, 4, 5, 6, 7, 8, ...0[]];

// --- The "Paths" Type with a new Depth Limit (D) ---
/**
 * Generates all dot-notation paths for leaf nodes in an object.
 * @param D - A recursion depth counter to prevent infinite loops.
 */
type Paths<T, D extends number = 5> =
    // Base case: If our depth counter hits 'never', stop immediately.
    [D] extends [never] ? never :

    T extends readonly unknown[] ?
      | `${number}`
      | `${number}.${Paths<T[number], Prev[D]>}`
    : T extends object ? {
        [K in KnownKeys<T> & (string | number)]:
            T[K] extends object
                // Pass the decremented depth counter `Prev[D]` here as well.
                ? `${K}` | `${K}.${Paths<T[K], Prev[D]>}`
                : `${K}`
    }[KnownKeys<T> & (string | number)]

    : never;


export type Split<S extends string, D extends string = "."> =
    S extends `${infer Head}${D}${infer Tail}`
        ? [Head, ...Split<Tail>]
        : [S];

type Get<T, K extends string> =
    K extends keyof T
    ? T[K]
    : K extends `${infer N extends number}`
        ? N extends keyof T ? T[N] : never
        : never;

type PathValue<T, Keys extends string[]> =
    Keys extends [infer K, ...infer Rest]
    ? K extends string
        ? Rest extends string[]
            ? PathValue<Get<T, K>, Rest>
            : never
        : never
    : T;

export type PathValueFromString<T, P extends Paths<T>> =
    P extends never
        ? never
        : PathValue<T, Split<P>>


/**
 * A generic type that finds all field paths in any object T
 * that point to a value of a specific type.
 * @template T The object to analyze.
 * @template TargetType The type of value you want to filter for.
 */
export type PathsOfType<T extends object, TargetType> = {
    [P in Paths<T>]: PathValueFromString<T, P> extends TargetType ? P : never
}[Paths<T>];


export type PathValueExact<T, P extends Paths<T>, V> =
    PathValueFromString<T, P> extends V ? PathValueFromString<T, P> : never;


/**
 * 递归地将类型 T 的所有属性及其子属性都变为可选的。
 * @template T - 需要被转换的类型。
 */
export type DeepPartial<T> =
    T extends Function ? T :
    T extends Array<infer U> ? Array<DeepPartial<U>> :
    T extends ReadonlyArray<infer U> ? ReadonlyArray<DeepPartial<U>> :
    T extends object ? {
        [P in keyof T]?: DeepPartial<T[P]>
    } :
    T;


/**
 * 递归地将类型 T 的所有属性及其子属性设为必填。
 */
export type DeepRequired<T> =
    T extends Function ? T :
    T extends Array<infer U> ? Array<DeepRequired<U>> :
    T extends ReadonlyArray<infer U> ? ReadonlyArray<DeepRequired<U>> :
    T extends object ? {
        [P in keyof T]-?: DeepRequired<T[P]>
    } :
    T;


/**
 * 递归地将类型 T 的所有属性设为 readonly。
 */
export type DeepReadonly<T> =
    T extends Function ? T :
    T extends Array<infer U> ? ReadonlyArray<DeepReadonly<U>> :
    T extends object ? {
        readonly [P in keyof T]: DeepReadonly<T[P]>
    } :
    T;


/**
 * 递归地将类型 T 的所有属性设为可为 null（包括嵌套属性）。
 */
export type DeepNullable<T> =
    T extends Function ? T :
    T extends Array<infer U> ? Array<DeepNullable<U> | null> | null :
    T extends ReadonlyArray<infer U> ? ReadonlyArray<DeepNullable<U>> :
    T extends object ? {
        [P in keyof T]: DeepNullable<T[P]> | null
    } :
    T | null;


/**
 * 递归地移除类型 T 中的 null 和 undefined。
 */
export type DeepNonNullable<T> =
    T extends Function ? T :
    T extends Array<infer U> ? Array<DeepNonNullable<NonNullable<U>>> :
    T extends ReadonlyArray<infer U> ? ReadonlyArray<DeepNonNullable<NonNullable<U>>> :
    T extends object ? {
        [P in keyof T]-?: DeepNonNullable<NonNullable<T[P]>>
    } : NonNullable<T>;



export type IsTuple<T> = T extends readonly [...infer _]
    ? number extends T["length"] ? false : true
    : false;


export type IsExactly<T, U> = U extends infer K ? ([T] extends [K]
    ? [K] extends [T] ? true : false
    : false) : never;


type IsExtendable<T, U> = unknown extends U ? (
	unknown extends T ? true : false
) : [T] extends [U] ? true : false


export type OmitTypes<T, K> =
    T extends Function ? T :
    IsTuple<T> extends true ? {
        [I in keyof T]: IsExtendable<T[I], K> extends true ? never : OmitTypes<T[I], K>
    } :
    T extends Array<infer U> ? Array<OmitTypes<U, K>> :
    T extends ReadonlyArray<infer U> ? ReadonlyArray<OmitTypes<U, K>> :
    T extends object ? {
        [Key in KnownKeys<T> as IsExtendable<T[Key], K> extends true ? never : Key]: OmitTypes<T[Key], K>
    } :
    T;
