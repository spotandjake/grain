const path = require("path");
const { execSync } = require("child_process");
const fs = require("fs");

const grainc_bin_name = "grainc";
const graindoc_bin_name = "graindoc";
const grainformat_bin_name = "grainformat";
const grainlsp_bin_name = "grainlsp";
const grainrun_bin_name = "grainrun";

// Helpers
const flagsFromOptions = (program, options) =>
  program.options
    .map((opt) => {
      if (!opt.forward) return null;
      const flag = opt.toFlag(options);
      if (flag) return flag;
      return null;
    })
    .filter((f) => f !== null);
// Exec
function exec(command, execOpts) {
  try {
    execSync(command, execOpts);
    return true;
  } catch (err) {
    process.exitCode = err.status;
    return false;
  }
}
function getGrainBin(bin) {
  const binPath = path.join(__dirname, `${bin}.exe`);
  // TODO: Maybe make an installable path & check it?
  if (process.pkg || bin == grainrun_bin_name || !fs.existsSync(binPath)) {
    const node = process.execPath;
    const binJs = path.join(__dirname, `${bin}.js`);
    return `"${node}" ${binJs}`;
  }
  return `${binPath}`;
}
function buildGrainBinExec(bin) {
  return (
    commandOrFile = "",
    options,
    program,
    execOpts = { stdio: "inherit" }
  ) => {
    const flags = flagsFromOptions(program, options);
    return exec(`${bin} ${flags.join(" ")} ${commandOrFile}`, execOpts);
  };
}
const execGrainc = buildGrainBinExec(getGrainBin(grainc_bin_name));
const execGraindoc = buildGrainBinExec(getGrainBin(graindoc_bin_name));
const execGrainformat = buildGrainBinExec(getGrainBin(grainformat_bin_name));
const execGrainlsp = (options, program, execOpts = { stdio: "inherit" }) => {
  const bin = getGrainBin(grainlsp_bin_name);
  const flags = flagsFromOptions(program, options);
  return exec(`${bin} ${flags.join(" ")}`, execOpts);
};
const grainrun = getGrainBin(grainrun_bin_name);
function execGrainrun(
  unprocessedArgs,
  file,
  options,
  program,
  execOpts = { stdio: "inherit" }
) {
  const preopens = {};
  options.dir?.forEach((preopen) => {
    const [guestDir, hostDir = guestDir] = preopen.split("=");
    preopens[guestDir] = hostDir;
  });

  const cliEnv = {};
  options.env?.forEach((env) => {
    const [name, ...rest] = env.split("=");
    const val = rest.join("=");
    cliEnv[name] = val;
  });

  const env = {
    ENV_VARS: JSON.stringify(cliEnv),
    PREOPENS: JSON.stringify(preopens),
    NODE_OPTIONS: `--experimental-wasi-unstable-preview1 --no-warnings`,
  };

  try {
    exec(`${grainrun} ${file} ${unprocessedArgs.join(" ")}`, {
      ...execOpts,
      env,
    });
  } catch (e) {
    process.exit(e.status);
  }
}
// Exports
module.exports = {
  grainc: execGrainc,
  graindoc: execGraindoc,
  grainformat: execGrainformat,
  grainlsp: execGrainlsp,
  grainrun: execGrainrun,
};
