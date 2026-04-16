export default [
    {
        files: ["**/*.js"],
        languageOptions: {
            ecmaVersion: 2022,
            sourceType: "commonjs",
            globals: {
                require: "readonly",
                module: "readonly",
                console: "readonly",
                __dirname: "readonly",
                process: "readonly"
            }
        },
        rules: {
            "no-undef": "error",
            "no-unreachable": "error",
            "no-fallthrough": "error",
            "no-unused-vars": "warn",
            "no-constant-condition": "error",
            "eqeqeq": "warn"
        }
    }
];
