import { createAppAuth } from "@octokit/auth-app";
import { Octokit } from "@octokit/rest";

interface IEnv {
  BOT_ID: string;
  BOT_KEY: string;
  BOT_INSTALLATION_ID: string;
  BOT_CLIENT_ID: string;
  BOT_CLIENT_SECRET: string;
}

async function main() {
  const env = (process.env as unknown) as IEnv;

  const octo = new Octokit({
    authStrategy: createAppAuth,
    auth: {
      id: Number(env.BOT_ID),
      privateKey: env.BOT_KEY,
      installationId: Number(env.BOT_INSTALLATION_ID),
      clientId: env.BOT_CLIENT_ID,
      clientSecret: env.BOT_CLIENT_SECRET,
    },
  });

  const auth = await octo.apps.createInstallationAccessToken({
    installation_id: Number(env.BOT_INSTALLATION_ID),
  });
  console.log(`::set-env name=BOT_TOKEN::${auth.data.token}`);
}

function handle() {
  return;
}

if (require.main === module) main().then(handle).catch(handle);
