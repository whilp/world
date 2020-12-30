import { createAppAuth } from "@octokit/auth-app";
import { Octokit } from "@octokit/rest";

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
    return;
  }

  const octo = new Octokit({
    authStrategy: createAppAuth,
    auth: {
      id: Number(env.BOT_ID),
      privateKey: env.BOT_KEY,
    },
  });

  const {
    data: { id: installationId },
  } = await octo.request("GET /repos/{repository}/installation", {
    repository: env.GITHUB_REPOSITORY,
  });

  const auth = await octo.apps.createInstallationAccessToken({
    installation_id: Number(installationId),
  });

  console.log(`::set-output name=token::${auth.data.token}`);
}

function handle() {
  return;
}

main().then(handle).catch(handle);
