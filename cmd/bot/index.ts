import { createAppAuth } from "@octokit/auth-app";
import { Octokit } from "@octokit/rest";
import * as core from "@actions/core";

interface IEnv {
  BOT_ID: string;
  BOT_KEY: string;
  CHECK: string;
  GITHUB_REPOSITORY: string;
}

async function main() {
  const env = (process.env as unknown) as IEnv;

  if (env.CHECK === "yes") {
    console.log("ok");
    return "ok";
  }
  const [owner, repo] = env.GITHUB_REPOSITORY.split("/");
  const app = createAppAuth({ appId: env.BOT_ID, privateKey: env.BOT_KEY });
  const authApp = await app({ type: "app" });
  const appAuthOcto = new Octokit({
    auth: authApp.token,
  });

  const {
    data: { id: installationId },
  } = await appAuthOcto.request("GET /repos/{owner}/{repo}/installation", {
    owner: owner,
    repo: repo,
  });

  const installationAuth = await app({ installationId, type: "installation" });

  return installationAuth.token;
}

function succeed(token: string) {
  core.setSecret(token);
  core.setOutput("token", token);
}

function fail(error: Error) {
  core.setFailed(error.message);
}

main().then(succeed).catch(fail);
