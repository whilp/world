def labels(extra):
    labels = {
        "org.opencontainers.image.authors": "https://github.com/whilp",
        "org.opencontainers.image.url": "https://github.com/whilp/world",
        "org.opencontainers.image.source": "https://github.com/whilp/world",
        "org.opencontainers.image.documentation": "https://github.com/whilp/world/#readme",
        "org.opencontainers.image.licenses": "MIT",
    }
    labels.update(extra)
    return labels
