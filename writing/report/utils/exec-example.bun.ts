import { cc } from "bun:ffi";


export const {
  symbols: { main },
} = cc({
  source: `${import.meta.dir}/../examples/twos-inv.c`,
  symbols: {
    main: {
      returns: "int",
      args: [],
    },
  },
});

main();
