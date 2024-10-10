import { cc } from "bun:ffi";


export const {
  symbols: { main },
} = cc({
  source: `${import.meta.dir}/../examples/bitwise-complement.c`,
  symbols: {
    main: {
      returns: "int",
      args: [],
    },
  },
});

main();
