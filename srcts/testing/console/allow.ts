// The setup of a package that holds an exception, which the tests of this
// directory run with (./vitest.config.ts): an error that another package sets
// off, until that package fixes it.

import { allowMessage } from "../../stubs/setup.ts";

allowMessage((message) => message.startsWith("An error that another package fixes later"));
