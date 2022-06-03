import stable_baselines3
import wandb


class WandbKVWriter(stable_baselines3.common.logger.KVWriter):
    def write(self, key_values, key_excluded, step=0):
        wandb.log(data=key_values, step=step)
        return

    def close(self):
        return


def setWandbLogger(model):
    logger = stable_baselines3.common.utils.configure_logger(verbose=1)
    logger.output_formats.append(WandbKVWriter())
    model.set_logger(logger)
