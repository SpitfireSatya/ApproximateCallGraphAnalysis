/*  */

import fs from 'fs';
import os from 'os';
import path from 'path'; 

export function expandPath(somePath) {
  const tildeExpandedPath = somePath.startsWith('~')
    ? somePath.replace(/^~/, os.homedir())
    : somePath;
  return path.resolve(tildeExpandedPath);
}

export const isWindows = /^Windows/.test(os.type());

export function srcPathsFromClasspathStrings(cpStrs) {
  return cpStrs.reduce((ret, colonSepPaths) => {
    const paths = colonSepPaths.split(path.delimiter);

    ret.push(...paths);
    return ret;
  }, []);
}

export function isWhitespace(s) {
  return s.trim() === '';
}

// eslint-disable-next-line consistent-return
function ensureDirInner(dir) {
  if (!fs.existsSync(dir)) {
    return fs.mkdirSync(dir);
  }

  const stats = fs.statSync(dir);

  if (!stats.isDirectory()) {
    throw new Error(`${dir} exists but is not a directory.`);
  }
}

export function ensureDir(dir) {
  const dirs = dir.split(/(\/[^\/]+)/); // eslint-disable-line no-useless-escape
  const len = dirs.length;

  for (let i = 0; i < len; i += 1) {
    if (!isWhitespace(dirs[i])) {
      ensureDirInner(dirs.slice(0, i + 1).join(''));
    }
  }
}

export function currentTimeMicros() {
  const [secs, nanos] = process.hrtime();
  // eslint-disable-next-line no-mixed-operators
  return (secs * 1e9 + nanos) / 1e3;
}

function mavenCoordinatesToPath(
  dependency,
  localRepo = path.join(os.homedir(), '.m2/repository'),
) {
  const groupArtifact = dependency.split('/');

  let group;
  let artifact;

  if (groupArtifact.length === 1) {
    // group and artifact are the same
    const actualGroupArtifact = groupArtifact[0].split(':')[0];
    [group, artifact] = [actualGroupArtifact, actualGroupArtifact];
  } else {
    [group, artifact] = [groupArtifact[0], groupArtifact[1].split(':')[0]];
  }

  let version = dependency.split(':')[1];

  // we weren't passed a version, search for the latest available locally
  if (version == null) {
    const versions = fs.readdirSync(
      path.join(expandPath(localRepo), ...group.split('.'), artifact),
    );

    version = versions.reduce(
      (a, b) => {
        if (global.goog.string.compareVersions(a, b) < 0) {
          return b;
        }
        return a;
      },
    );
  }

  return path.join(
    expandPath(localRepo),
    ...group.split('.'),
    artifact,
    version,
    `${artifact}-${version}.jar`,
  );
}

export function srcPathsFromMavenDependencies(
  dependencies,
  localRepo,
) {
  return dependencies.reduce((ret, commaSeparatedDeps) => {
    const paths = commaSeparatedDeps
      .split(',')
      .map((dep) => mavenCoordinatesToPath(dep, localRepo));

    ret.push(...paths);
    return ret;
  }, []);
}

