def deb():
    native.http_file(name = "ubuntu_binutils", sha256 = "47e684ddebf2c373da896d3159580aece2fb47af0ab477e408d4a44e2605c386", url = "http://archive.ubuntu.com/ubuntu/pool/main/b/binutils/binutils_2.30-21ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_binutils_common", sha256 = "53a4a0aa5090d737c7cb343b4add84ab2ade1a361c1800a3b8323a375c11ebea", url = "http://archive.ubuntu.com/ubuntu/pool/main/b/binutils/binutils-common_2.30-21ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_binutils_x86_64_linux_gnu", sha256 = "af63c820509040063d6236be17059254ed6342222c9534223f3f9f2dde60bd4d", url = "http://archive.ubuntu.com/ubuntu/pool/main/b/binutils/binutils-x86-64-linux-gnu_2.30-21ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_build_essential", sha256 = "bb2ac7aae21544446f45e200aacb961f0dcc42c8caacdc8da37d392b222abfa0", url = "http://archive.ubuntu.com/ubuntu/pool/main/b/build-essential/build-essential_12.4ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_ca_certificates", sha256 = "195ffe05ae7d060146f890b1db225f5e8c3a8c505ffbea1fba30a520a1cd58d8", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/ca-certificates/ca-certificates_20180409_all.deb")
    native.http_file(name = "ubuntu_cpp", sha256 = "8c37f4a0db4ebd450fe25398030b4b926939940c8673771b461a06f954552287", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-defaults/cpp_7.3.0-3ubuntu2.1_amd64.deb")
    native.http_file(name = "ubuntu_cpp_7", sha256 = "89105cd9825675008861431b5a967bbc14a82c2f3c23ae3b19427a81d659af4b", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/cpp-7_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_curl", sha256 = "5b55218535734d954273995d2e1fc651475fa2dbbe2128bb4c0f3209b7a0e512", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/curl/curl_7.58.0-2ubuntu3.3_amd64.deb")
    native.http_file(name = "ubuntu_dbus", sha256 = "9d1dc163856f03c8d468d5e18d073bfffc2b1f160829300c1fcfea00173985b3", url = "http://archive.ubuntu.com/ubuntu/pool/main/d/dbus/dbus_1.12.2-1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_dh_python", sha256 = "fdb04c16cbf229b976eae2b9c4c5c488d8d34b8a8d4fbf4555ab15898ac06161", url = "http://archive.ubuntu.com/ubuntu/pool/main/d/dh-python/dh-python_3.20180325ubuntu2_all.deb")
    native.http_file(name = "ubuntu_dirmngr", sha256 = "1b150de9ee8187bf935da550fb4fae29738740f57434d830172fea14cfdf4c78", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/dirmngr_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_dmsetup", sha256 = "9d30ebcba80d27fc8b94aae4f71a8b32623897f3a31da9c0fcf08e3da561ebf3", url = "http://archive.ubuntu.com/ubuntu/pool/main/l/lvm2/dmsetup_1.02.145-4.1ubuntu3_amd64.deb")
    native.http_file(name = "ubuntu_dpkg_dev", sha256 = "d5bf06c6cd79f51692bbd2bf139773342ee1f77739da4fe93ccd42b6602e85ee", url = "http://archive.ubuntu.com/ubuntu/pool/main/d/dpkg/dpkg-dev_1.19.0.5ubuntu2_all.deb")
    native.http_file(name = "ubuntu_fakeroot", sha256 = "cccf6935009674e9aa1d0fc8f1516c7d69967a32c4f8ef368a91d39d0d68e57b", url = "http://archive.ubuntu.com/ubuntu/pool/main/f/fakeroot/fakeroot_1.22-2ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_file", sha256 = "9ad57da60104eee39bcd0981e9bb0bd7b83d1b1623571156d8d56d2249aa6041", url = "http://archive.ubuntu.com/ubuntu/pool/main/f/file/file_5.32-2ubuntu0.1_amd64.deb")
    native.http_file(name = "ubuntu_gPP", sha256 = "0e0d83b12201f3430542b6f75ff5072c1f8e6480dcdd2b84fb5ae93ba0445a09", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-defaults/g++_7.3.0-3ubuntu2.1_amd64.deb")
    native.http_file(name = "ubuntu_gPP_7", sha256 = "5ef39be3d9d07733ddb4f99c93af29f9d83d3ebfe35732a1f23bc372ce0958b3", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/g++-7_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_gcc", sha256 = "3468339cf5e4fb303643feb5e34f257f342041745709b56b9ae2684c7d2d5518", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-defaults/gcc_7.3.0-3ubuntu2.1_amd64.deb")
    native.http_file(name = "ubuntu_gcc_7", sha256 = "7d639297e4cf724ce723f48b07696f2d4ad55d0d677b9dc49a48a905f5c73af2", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/gcc-7_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_gcc_7_base", sha256 = "0e120fbe843926b1a6187887326b61e384a15a43b4888dfc4f996193d5e14f47", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/gcc-7-base_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_gir1_2_glib_2_0", sha256 = "1dffd1d91b80785060c12661beeb551af46a0f0ccd42ab22a778548c5883a750", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gobject-introspection/gir1.2-glib-2.0_1.56.1-1_amd64.deb")
    native.http_file(name = "ubuntu_git", sha256 = "fe2da58d0dc70cf3224d58ec64ccad4bcc17d70723ce97cc6415228146b3cafa", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/git/git_2.17.1-1ubuntu0.3_amd64.deb")
    native.http_file(name = "ubuntu_git_man", sha256 = "a78f0f941932bab0c1d1ec8298a6113919f90a8527b079a2d07e4f47bb939f86", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/git/git-man_2.17.1-1ubuntu0.3_all.deb")
    native.http_file(name = "ubuntu_gnupg", sha256 = "8ba2ff0e5c0e83a2fbb98cc7296f9e92a6c21bb38add257a7feba8f0b98b137b", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gnupg_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_gnupg_l10n", sha256 = "a5b124f181810e4ee356c9856e0ce6ba67407590fb3feae1f70c4885a17fa4c1", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gnupg-l10n_2.2.4-1ubuntu1.1_all.deb")
    native.http_file(name = "ubuntu_gnupg_utils", sha256 = "80ead54947c9c08ac606c5f70f3a6a90dc1cccdca37412c00d6061f8ccf13e31", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gnupg-utils_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_gpg", sha256 = "521f3a866d5ea3f04ed4cc1c1d3c4c146945a69b8b51c87c36b0ca961e683ebe", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gpg_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_gpg_agent", sha256 = "8d906d8c6044190b4d88c23384919d18b7a85b1985c93fb4899eef3b6ef2d096", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gpg-agent_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_gpg_wks_client", sha256 = "fb45f5ae38856d59df23bafacf66ff5941eccdf605b7af24db854e5b710a2389", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gpg-wks-client_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_gpg_wks_server", sha256 = "fcf69a25cb92d2de4abf1e9957ec43119bbef476235364a6ef94917c12173df6", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gpg-wks-server_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_gpgconf", sha256 = "e4bdcdfca97094fa7d14bdd54fdd8fb88de21aebe6f2a1900caa66c7baae55c0", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gpgconf_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_gpgsm", sha256 = "2b0075f37f8de84918eb39a7e9874a2fe1ad9456d35557299a1a17dd24ca7155", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gnupg2/gpgsm_2.2.4-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_krb5_locales", sha256 = "860565aad204bf93a3def57e13974dfe573046d457df269756810aba2fe02543", url = "http://archive.ubuntu.com/ubuntu/pool/main/k/krb5/krb5-locales_1.16-2build1_all.deb")
    native.http_file(name = "ubuntu_less", sha256 = "00d9c5e56b81c95a14e3fd148ff464acaf3fe757d3e4b4d1e54990b28c54eb57", url = "http://archive.ubuntu.com/ubuntu/pool/main/l/less/less_487-0.1_amd64.deb")
    native.http_file(name = "ubuntu_libalgorithm_diff_perl", sha256 = "e68d641c28c22736068edc4c455afd52a7928ef5cac75d33a80c1b5b7f26f17b", url = "http://archive.ubuntu.com/ubuntu/pool/main/liba/libalgorithm-diff-perl/libalgorithm-diff-perl_1.19.03-1_all.deb")
    native.http_file(name = "ubuntu_libalgorithm_diff_xs_perl", sha256 = "a94fa09306a4f7225a4bb69297fd2cc72a2f1139e1b6f9d88a65f1fdf6a53218", url = "http://archive.ubuntu.com/ubuntu/pool/main/liba/libalgorithm-diff-xs-perl/libalgorithm-diff-xs-perl_0.04-5_amd64.deb")
    native.http_file(name = "ubuntu_libalgorithm_merge_perl", sha256 = "ff7a1922d430da383cc1ee20bee8fe464fb56605297dcbb3412338a356cc7ad6", url = "http://archive.ubuntu.com/ubuntu/pool/main/liba/libalgorithm-merge-perl/libalgorithm-merge-perl_0.08-3_all.deb")
    native.http_file(name = "ubuntu_libapparmor1", sha256 = "3c3f4bc1e686bf3ded1116ec59d15f63d2c5ced0c11f6065fc55218628b83517", url = "http://archive.ubuntu.com/ubuntu/pool/main/a/apparmor/libapparmor1_2.12-4ubuntu5.1_amd64.deb")
    native.http_file(name = "ubuntu_libargon2_0", sha256 = "3f66dc6094e4173a559183da65a06fe1be235c1baa268d7145d2abc813164b50", url = "http://archive.ubuntu.com/ubuntu/pool/main/a/argon2/libargon2-0_0~20161029-1.1_amd64.deb")
    native.http_file(name = "ubuntu_libasan4", sha256 = "3b408b3f2406cec4cf5d3eb561de8275f8406346b3d37a85b07b506c20be4a23", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/libasan4_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libasn1_8_heimdal", sha256 = "2a0b781aca01c503b4655567edc505be8adf1274c4fc0a2e07ead4bda4098b8a", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libasn1-8-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libassuan0", sha256 = "190df07d7983b062cb70b86b8d88346cafbcba44a86cb8fcf7005fb56e5b29bf", url = "http://archive.ubuntu.com/ubuntu/pool/main/liba/libassuan/libassuan0_2.5.1-2_amd64.deb")
    native.http_file(name = "ubuntu_libatomic1", sha256 = "74db5fdd8050f2e1673e866b8779f1dbc50740ba09260e394a82932dc9a5b1a6", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/libatomic1_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libbinutils", sha256 = "9e7a7791b4f7d2a7b2ea0a69a1c76cbb93834833b0cb32a9865d7858b2eaa2de", url = "http://archive.ubuntu.com/ubuntu/pool/main/b/binutils/libbinutils_2.30-21ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libbsd0", sha256 = "b73172c736941942e8893db6b7307e6525b05b9601827238f5c4a6d59d44f786", url = "http://archive.ubuntu.com/ubuntu/pool/main/libb/libbsd/libbsd0_0.8.7-1_amd64.deb")
    native.http_file(name = "ubuntu_libc6_dev", sha256 = "e426c70a940a7d0c5c95823a5fd01f26bd8bcb08d109df2f8c96c439da8dc440", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/glibc/libc6-dev_2.27-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libc_dev_bin", sha256 = "69ea1317b37cbd467eb7d216f5d23aa8831d926908e9e12477aa28bdc1d5e62b", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/glibc/libc-dev-bin_2.27-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libcap2", sha256 = "00655717ec2494a953b1c3f477bc86b080760402b72a926057076144aa8b9c41", url = "http://archive.ubuntu.com/ubuntu/pool/main/libc/libcap2/libcap2_2.25-1.2_amd64.deb")
    native.http_file(name = "ubuntu_libcc1_0", sha256 = "d33deb6362802430cf7024baf870710982336246cb826a98368b42dbb4d7bf3d", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/libcc1-0_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libcilkrts5", sha256 = "994578071ae19337d3125b7219042a9a1d870b6c058e3fe82ce1741e0185d38b", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/libcilkrts5_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libcryptsetup12", sha256 = "a6bf9a310a34104a449dbceeee943a1a7d69721103af04955b8a26848488ec12", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/cryptsetup/libcryptsetup12_2.0.2-1ubuntu1.1_amd64.deb")
    native.http_file(name = "ubuntu_libcurl3_gnutls", sha256 = "51000079858b7f20e38f1b4371e6b0fec472a2c8a6fc98d0b32b95497fdbae79", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/curl/libcurl3-gnutls_7.58.0-2ubuntu3.3_amd64.deb")
    native.http_file(name = "ubuntu_libcurl4", sha256 = "6eae764bbc75334390531eea06b461bd5a7ffed6f76ad7e4b358955fb6a2d765", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/curl/libcurl4_7.58.0-2ubuntu3.3_amd64.deb")
    native.http_file(name = "ubuntu_libdbus_1_3", sha256 = "a87d9e4af516cfc6ce04957d291a36347863939b4a61cbe4f16f1235d7b31b72", url = "http://archive.ubuntu.com/ubuntu/pool/main/d/dbus/libdbus-1-3_1.12.2-1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libdevmapper1_02_1", sha256 = "6ba9c0edd44421480af99ca8115d08197a541d9ac5e0c031cc650b5960d6bbd8", url = "http://archive.ubuntu.com/ubuntu/pool/main/l/lvm2/libdevmapper1.02.1_1.02.145-4.1ubuntu3_amd64.deb")
    native.http_file(name = "ubuntu_libdpkg_perl", sha256 = "2684524345c50525fcab9a65c828a5bdf2561920a3041fbe36bd42df6e991bb7", url = "http://archive.ubuntu.com/ubuntu/pool/main/d/dpkg/libdpkg-perl_1.19.0.5ubuntu2_all.deb")
    native.http_file(name = "ubuntu_libedit2", sha256 = "414cc28beac456b78140b0a07558034c4a2987212b347401dbb6e1d375f11d32", url = "http://archive.ubuntu.com/ubuntu/pool/main/libe/libedit/libedit2_3.1-20170329-1_amd64.deb")
    native.http_file(name = "ubuntu_liberror_perl", sha256 = "1fcf30b7f6dcd9981cd43d61b8edceff370ab5e565177758715ca905ae61aefe", url = "http://archive.ubuntu.com/ubuntu/pool/main/libe/liberror-perl/liberror-perl_0.17025-1_all.deb")
    native.http_file(name = "ubuntu_libexpat1", sha256 = "202db01e1b4204b6e34e44b4f4a6ba6b3928089c2fa728db86e13cd4b8548191", url = "http://archive.ubuntu.com/ubuntu/pool/main/e/expat/libexpat1_2.2.5-3_amd64.deb")
    native.http_file(name = "ubuntu_libexpat1_dev", sha256 = "a0ac6b9fd81e8b45569a98ee4d857d28b44bb44cc98589aa987b113156cfb69d", url = "http://archive.ubuntu.com/ubuntu/pool/main/e/expat/libexpat1-dev_2.2.5-3_amd64.deb")
    native.http_file(name = "ubuntu_libfakeroot", sha256 = "5c11c66e7e8b58aff352e9141661422b18beced513dbf2b923c17fc7ec5c5ee8", url = "http://archive.ubuntu.com/ubuntu/pool/main/f/fakeroot/libfakeroot_1.22-2ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libfile_fcntllock_perl", sha256 = "138111d1f1e5969227633ff67e7279b31b8ff6a31d7c5b1ad2ad86f69e8ef2b4", url = "http://archive.ubuntu.com/ubuntu/pool/main/libf/libfile-fcntllock-perl/libfile-fcntllock-perl_0.22-3build2_amd64.deb")
    native.http_file(name = "ubuntu_libgcc_7_dev", sha256 = "d7a9e944b8de456eff7fd23a8cbedefedb712cd3ec42e0b7239df9f943d73fa2", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/libgcc-7-dev_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libgdbm5", sha256 = "d12d02c0d237e2ff766c02e31f4779a455c81d9f7a8ebbc7a083cc2d77d5cf13", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gdbm/libgdbm5_1.14.1-6_amd64.deb")
    native.http_file(name = "ubuntu_libgdbm_compat4", sha256 = "fb66aa765ad0527762fa62b7536cc1b577a5ddaf3f856616f34b05b578f92664", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gdbm/libgdbm-compat4_1.14.1-6_amd64.deb")
    native.http_file(name = "ubuntu_libgirepository_1_0_1", sha256 = "5ebe2f31f36042bec9c390e8e890b24a3a1f21ec1b2f70eabee62c3bf63fd511", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gobject-introspection/libgirepository-1.0-1_1.56.1-1_amd64.deb")
    native.http_file(name = "ubuntu_libglib2_0_0", sha256 = "10ad56647b16eb4f4134d6d432a70a1a5d8cb4df2201e53ef9109b04078e490c", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/glib2.0/libglib2.0-0_2.56.2-0ubuntu0.18.04.2_amd64.deb")
    native.http_file(name = "ubuntu_libglib2_0_data", sha256 = "77735d268205542a6f0283b02ce14677415391ddcab24e4f25c2b7a9bfa1aebb", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/glib2.0/libglib2.0-data_2.56.2-0ubuntu0.18.04.2_all.deb")
    native.http_file(name = "ubuntu_libgomp1", sha256 = "e17577c76e1ecd469e631a47b44c22ba80cb8647aa7baa5157cba58353181502", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/libgomp1_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libgssapi3_heimdal", sha256 = "1a8952506c468c9659e9eef297440ef6209063fd7c78153a71d97033761eb9f3", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libgssapi3-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libgssapi_krb5_2", sha256 = "faf51322f26debf0cc23222ba07e21e7b91af568895c5b7fb3dc6747321b8f07", url = "http://archive.ubuntu.com/ubuntu/pool/main/k/krb5/libgssapi-krb5-2_1.16-2build1_amd64.deb")
    native.http_file(name = "ubuntu_libhcrypto4_heimdal", sha256 = "328ee9b8506e543fe564bb3c3eff818363ad1efb03fbbb51fc8242b1240e2259", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libhcrypto4-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libheimbase1_heimdal", sha256 = "17d46bf93020834451b582e3cf584000bd2cdb16e10a2139e2cc024503309f3c", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libheimbase1-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libheimntlm0_heimdal", sha256 = "c5cb8de57d96a65e8d8bbc731acc113b1358cdfc9f5beca546def112926d99e8", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libheimntlm0-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libhx509_5_heimdal", sha256 = "99b3774d1df23ca1340352bbf5b23f50238b4022ca165b271ea1e94a68e257a3", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libhx509-5-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libicu60", sha256 = "7a2f29ea19f2a14007fd84f25eb88db4fb3ca43bdb07fdb681329d2b5daca500", url = "http://archive.ubuntu.com/ubuntu/pool/main/i/icu/libicu60_60.2-3ubuntu3_amd64.deb")
    native.http_file(name = "ubuntu_libidn11", sha256 = "f453173ab7715b33b42200724630384ebfe79c0b2dc74735fd35549fc11b6532", url = "http://archive.ubuntu.com/ubuntu/pool/main/libi/libidn/libidn11_1.33-2.1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libip4tc0", sha256 = "583990ffb2f9e0ea825d4ce3a7c6db0af4f3afc544114c5b6769d95ff5e0b310", url = "http://archive.ubuntu.com/ubuntu/pool/main/i/iptables/libip4tc0_1.6.1-2ubuntu2_amd64.deb")
    native.http_file(name = "ubuntu_libisl19", sha256 = "07a0827aba14140b1833ca19ced3f939b2d075646094926d43678a0d19cc942f", url = "http://archive.ubuntu.com/ubuntu/pool/main/i/isl/libisl19_0.19-1_amd64.deb")
    native.http_file(name = "ubuntu_libitm1", sha256 = "90365dff90c78b63af3fe2f0b32387172137d0b5770883002eafd103899ca3c7", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/libitm1_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libjson_c3", sha256 = "2a763576409ea30db729c107b7486f031a6ef296daf26fa06b55068371c5f8cb", url = "http://archive.ubuntu.com/ubuntu/pool/main/j/json-c/libjson-c3_0.12.1-1.3_amd64.deb")
    native.http_file(name = "ubuntu_libk5crypto3", sha256 = "183b5cc62c875f62efa342ec541254c8e0ba95e98e31d30ff33abd59743da9f1", url = "http://archive.ubuntu.com/ubuntu/pool/main/k/krb5/libk5crypto3_1.16-2build1_amd64.deb")
    native.http_file(name = "ubuntu_libkeyutils1", sha256 = "0ba4ab4a5b6510de82a45a2ed69250b866f330bb761f2d478b6382a402c889f0", url = "http://archive.ubuntu.com/ubuntu/pool/main/k/keyutils/libkeyutils1_1.5.9-9.2ubuntu2_amd64.deb")
    native.http_file(name = "ubuntu_libkmod2", sha256 = "2109ee77049263c5b206135cc8870d6f66f60e27dcc3767acb1e79a17f6384fe", url = "http://archive.ubuntu.com/ubuntu/pool/main/k/kmod/libkmod2_24-1ubuntu3_amd64.deb")
    native.http_file(name = "ubuntu_libkrb5_26_heimdal", sha256 = "375544daaee0c81460d8be276bf6c382eb2e8fb1ca197599a82263d025f81844", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libkrb5-26-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libkrb5_3", sha256 = "7161590a7217f84db01f3cf5aae2a80f75e9070495222d25dd3190f502c77e97", url = "http://archive.ubuntu.com/ubuntu/pool/main/k/krb5/libkrb5-3_1.16-2build1_amd64.deb")
    native.http_file(name = "ubuntu_libkrb5support0", sha256 = "1f908193bd1f6a8bc0525b96132ba335c66edd1f82a3015e6ab6ed605ee573aa", url = "http://archive.ubuntu.com/ubuntu/pool/main/k/krb5/libkrb5support0_1.16-2build1_amd64.deb")
    native.http_file(name = "ubuntu_libksba8", sha256 = "d1631736f3c1ae4bb10a93f96c7c8bd21344e76be1b0c888e9420e4131983201", url = "http://archive.ubuntu.com/ubuntu/pool/main/libk/libksba/libksba8_1.3.5-2_amd64.deb")
    native.http_file(name = "ubuntu_libldap_2_4_2", sha256 = "f5a1338180e0fa8b664c1038ad6e7ed5c9837b84e867032e218a80f9c2e8dc8d", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openldap/libldap-2.4-2_2.4.45+dfsg-1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libldap_common", sha256 = "7d0abd28060fadebe633bfb7ca5499137f10d5e1d0f0aa41c3d9f1d8bbe6811c", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openldap/libldap-common_2.4.45+dfsg-1ubuntu1_all.deb")
    native.http_file(name = "ubuntu_liblocale_gettext_perl", sha256 = "71e6c29eba41d285695402837222cadc53f39dbff4548adc6906b7f376ddcb31", url = "http://archive.ubuntu.com/ubuntu/pool/main/libl/liblocale-gettext-perl/liblocale-gettext-perl_1.07-3build2_amd64.deb")
    native.http_file(name = "ubuntu_liblsan0", sha256 = "07d84a83e3926a8a5089c4f7728c7afb3d9acaf22707f4afa4e68c5d792e1465", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/liblsan0_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libltdl7", sha256 = "57611d5d85126bb08e28f15e0aa0dfceeb33451494797fa7e3ecc6265301eba9", url = "http://archive.ubuntu.com/ubuntu/pool/main/libt/libtool/libltdl7_2.4.6-2_amd64.deb")
    native.http_file(name = "ubuntu_libmagic1", sha256 = "79722d51dc5accaff5b53dbf5dd5c47a47b1dd4fce3987284ddc79e6c45e315f", url = "http://archive.ubuntu.com/ubuntu/pool/main/f/file/libmagic1_5.32-2ubuntu0.1_amd64.deb")
    native.http_file(name = "ubuntu_libmagic_mgc", sha256 = "df2b1dfcc5be00600a6fd39e294fffab8ca4ef9cdd71e4a37174efcefd2d2b6b", url = "http://archive.ubuntu.com/ubuntu/pool/main/f/file/libmagic-mgc_5.32-2ubuntu0.1_amd64.deb")
    native.http_file(name = "ubuntu_libmpc3", sha256 = "3c3c45aa50ce1ff753107766c6fea35a82c52ad11afcab0abfd0cd0c76730f87", url = "http://archive.ubuntu.com/ubuntu/pool/main/m/mpclib3/libmpc3_1.1.0-1_amd64.deb")
    native.http_file(name = "ubuntu_libmpdec2", sha256 = "2962dbb81413ad47fee589bb6663589e38b84d80b4d55d8019f0b305025c5eb6", url = "http://archive.ubuntu.com/ubuntu/pool/main/m/mpdecimal/libmpdec2_2.4.2-1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libmpfr6", sha256 = "701990426b88d7af39237d6128078d8a4788c72d42e1b9a1b058a871e16ab7fb", url = "http://archive.ubuntu.com/ubuntu/pool/main/m/mpfr4/libmpfr6_4.0.1-1_amd64.deb")
    native.http_file(name = "ubuntu_libmpx2", sha256 = "4cc796d33fe51aa6dfadc5639f012d7ec1b377327baab40943c175a7925aeda5", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/libmpx2_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libnghttp2_14", sha256 = "6655e9e57e7191313836e1cd2c8ee988489ab98c6f8a425216c8f2bd6135834a", url = "http://archive.ubuntu.com/ubuntu/pool/main/n/nghttp2/libnghttp2-14_1.30.0-1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libnpth0", sha256 = "06e63fa7325b6f89090d9be52dcea560537baf35201d82976aa36cc5a25a3a19", url = "http://archive.ubuntu.com/ubuntu/pool/main/n/npth/libnpth0_1.5-3_amd64.deb")
    native.http_file(name = "ubuntu_libnss_systemd", sha256 = "33c28ad8eab5ef4708fba2964452353a0462bf9b632596b98e9479ac7fcdba6b", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/systemd/libnss-systemd_237-3ubuntu10.3_amd64.deb")
    native.http_file(name = "ubuntu_libpam_systemd", sha256 = "1d6dfd0b7f8594c4117b199fa922d0433769c3c4cefc653941ef8b280fb0ac58", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/systemd/libpam-systemd_237-3ubuntu10.3_amd64.deb")
    native.http_file(name = "ubuntu_libperl5_26", sha256 = "8d38855a40aea8dfb713c15e9eb1d21d5538478f0e2067a4b2dcb8d17e53414d", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/perl/libperl5.26_5.26.1-6ubuntu0.2_amd64.deb")
    native.http_file(name = "ubuntu_libpsl5", sha256 = "e629a5a55218956c4be5709b1afe0dc5e104dec6a5e6b7c4d12add4fd6c4f652", url = "http://archive.ubuntu.com/ubuntu/pool/main/libp/libpsl/libpsl5_0.19.1-5build1_amd64.deb")
    native.http_file(name = "ubuntu_libpython2_7", sha256 = "ed75fc8db56f9c63bf0646f1d8263f8f2f4a777d5872d7c6e8178ba73f8e1f36", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python2.7/libpython2.7_2.7.15~rc1-1_amd64.deb")
    native.http_file(name = "ubuntu_libpython2_7_dev", sha256 = "446e0dda0e694f73c90610c6884ca13e95246ce8a0859081c7e71d9c62562709", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python2.7/libpython2.7-dev_2.7.15~rc1-1_amd64.deb")
    native.http_file(name = "ubuntu_libpython2_7_minimal", sha256 = "68d9565c9dee6493d18e9b9446e32b5b49886f351a9dfc76c3b8a392687b542a", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python2.7/libpython2.7-minimal_2.7.15~rc1-1_amd64.deb")
    native.http_file(name = "ubuntu_libpython2_7_stdlib", sha256 = "d5a985d234b63e61d2cccfa01cda04e8813ad39b07da19ffaf5ee1fe89441dc4", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python2.7/libpython2.7-stdlib_2.7.15~rc1-1_amd64.deb")
    native.http_file(name = "ubuntu_libpython3_6", sha256 = "059d050ac118d423330279714f79b55299e5c166941205a24f8ecf579922a4a3", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3.6/libpython3.6_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libpython3_6_dev", sha256 = "d63e9f9b9986e9b203cb1ef1f2a7650d222e68db37e29ccc35fce695585d9b27", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3.6/libpython3.6-dev_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libpython3_6_minimal", sha256 = "4fa2c29863ae90639519cda058fb7712bf5c557dfdd40bca444e061b40117e61", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3.6/libpython3.6-minimal_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libpython3_6_stdlib", sha256 = "7e1d05616c31b44419935360aca95e79d9d472ca02f2cae394d1651a56bd0af7", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3.6/libpython3.6-stdlib_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libpython3_dev", sha256 = "f64da177f3ce1ef9ce3d4442b9915cb4f7ecd9b5a0feb96d8f5c7d20d2594a25", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3-defaults/libpython3-dev_3.6.5-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libpython3_stdlib", sha256 = "22e8d528ab2636408236270dea5e2f1ff03e3cf77d2f7f2fbe8378e725eb5072", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3-defaults/libpython3-stdlib_3.6.5-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_libquadmath0", sha256 = "8da0f82a0446a7b74772b16b43d632ec3afa4fe684b0740fe66f55e91806757a", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/libquadmath0_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libreadline7", sha256 = "8706403267e95615f1b70db31ff16709361982728b308466346e01c20a743dd5", url = "http://archive.ubuntu.com/ubuntu/pool/main/r/readline/libreadline7_7.0-3_amd64.deb")
    native.http_file(name = "ubuntu_libroken18_heimdal", sha256 = "387e00fbdf31384eb031cd3297c1cbec4d22b07e724d8ce2d8b219fdabac9ba8", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libroken18-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_librtmp1", sha256 = "1a1f9615c0b907b897cbbe61f4f6d29e395c9bb9bcfb94621fa73fe2fb8cb7d2", url = "http://archive.ubuntu.com/ubuntu/pool/main/r/rtmpdump/librtmp1_2.4+20151223.gitfa8646d.1-1_amd64.deb")
    native.http_file(name = "ubuntu_libsasl2_2", sha256 = "8ffbca3fe85061fbbfd7e78fb05466fd5364aacf2bb0b1c515279658229fe51a", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/cyrus-sasl2/libsasl2-2_2.1.27~101-g0780600+dfsg-3ubuntu2_amd64.deb")
    native.http_file(name = "ubuntu_libsasl2_modules", sha256 = "82bf018e7f7873d520cd91f393ca562aa8ec1cf1548aea44e45b5a623c92b73d", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/cyrus-sasl2/libsasl2-modules_2.1.27~101-g0780600+dfsg-3ubuntu2_amd64.deb")
    native.http_file(name = "ubuntu_libsasl2_modules_db", sha256 = "42b54ad3036dde9b1c9870187d8ac4e7e339536c3e9a253ae60727144bc7b813", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/cyrus-sasl2/libsasl2-modules-db_2.1.27~101-g0780600+dfsg-3ubuntu2_amd64.deb")
    native.http_file(name = "ubuntu_libsqlite3_0", sha256 = "4f3425ebf22f34f8e4d12958550993d82fa436eaa4a4b67ba35cc80491073580", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/sqlite3/libsqlite3-0_3.22.0-1_amd64.deb")
    native.http_file(name = "ubuntu_libssl1_0_0", sha256 = "bf09a35c2defdb6aac1c7edca1487416e6fbcaf70cca7a2770aab33a758410a6", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openssl1.0/libssl1.0.0_1.0.2n-1ubuntu5.1_amd64.deb")
    native.http_file(name = "ubuntu_libssl1_1", sha256 = "3ee2903ce5258430f1e91202cf2a5e14e62d78ed30204dd64f1c203e44464b7b", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openssl/libssl1.1_1.1.0g-2ubuntu4.1_amd64.deb")
    native.http_file(name = "ubuntu_libstdcPP_7_dev", sha256 = "1115bed1fc3c248a5716f808ca1f3f8ee74e66b94c1ae925d567adc46abe7962", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/libstdc++-7-dev_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libtsan0", sha256 = "4282530214f41bb6c0d03c55a42d4d5dc4cb887a77d1f5f44b203184d34607b1", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-8/libtsan0_8.2.0-1ubuntu2~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libubsan0", sha256 = "9da9a34b2de8383c2feaca57d2783ac683e22afbe96f8ab0715943810ce252c2", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/gcc-7/libubsan0_7.3.0-27ubuntu1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_libwind0_heimdal", sha256 = "164be8ab5054197c7a94e7731f5fd11f1268a8286c0aa452595fa9bacb15191d", url = "http://archive.ubuntu.com/ubuntu/pool/main/h/heimdal/libwind0-heimdal_7.5.0+dfsg-1_amd64.deb")
    native.http_file(name = "ubuntu_libwrap0", sha256 = "d7d273e017f992ad81f8e8c695ba8bff2de886cc98c63b10a2e44048527fa248", url = "http://archive.ubuntu.com/ubuntu/pool/main/t/tcp-wrappers/libwrap0_7.6.q-27_amd64.deb")
    native.http_file(name = "ubuntu_libx11_6", sha256 = "aedaa372b801a947aef451e9ff8c222a1ef55011601ad687c57233d0b90ea8c1", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libx11/libx11-6_1.6.4-3ubuntu0.1_amd64.deb")
    native.http_file(name = "ubuntu_libx11_data", sha256 = "1916c985e755a3851c89a4af66c7dfa2609e862adfa4d2d5da619e2b21b03340", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libx11/libx11-data_1.6.4-3ubuntu0.1_all.deb")
    native.http_file(name = "ubuntu_libxau6", sha256 = "881544ee71d85e89ebf224e45acc94930f6a309da2fb33dbf850bd1434e70597", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libxau/libxau6_1.0.8-1_amd64.deb")
    native.http_file(name = "ubuntu_libxcb1", sha256 = "f2ffa7be0f46b016a867dd4065fb27187f8055d50de0e5b89a690b557056349a", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libxcb/libxcb1_1.13-1_amd64.deb")
    native.http_file(name = "ubuntu_libxdmcp6", sha256 = "8905e3441f5e408208b73817253504498c4488c7803ff4a8fb85e225f0757e55", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libxdmcp/libxdmcp6_1.1.2-3_amd64.deb")
    native.http_file(name = "ubuntu_libxext6", sha256 = "05a861283b2aa54ee5f40e6844d0cc73f50c30440a28964da00302b7fbcecc9e", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libxext/libxext6_1.3.3-1_amd64.deb")
    native.http_file(name = "ubuntu_libxml2", sha256 = "c2397b4af4388c34f19e6182abd05cc149775b1676bd999c3fef15d97f3242ff", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libxml2/libxml2_2.9.4+dfsg1-6.1ubuntu1.2_amd64.deb")
    native.http_file(name = "ubuntu_libxmuu1", sha256 = "a9ed1eda3da60b95db804abea45d2aa882bfdd9edaa50974d292c2666b66fe0c", url = "http://archive.ubuntu.com/ubuntu/pool/main/libx/libxmu/libxmuu1_1.1.2-2_amd64.deb")
    native.http_file(name = "ubuntu_linux_libc_dev", sha256 = "ec5f486e00a3e47a2bd447443f8bc7b59b87c1656b6a957c65e9f306c6d1198a", url = "http://archive.ubuntu.com/ubuntu/pool/main/l/linux/linux-libc-dev_4.15.0-38.41_amd64.deb")
    native.http_file(name = "ubuntu_make", sha256 = "6a7f7b7ad1f6ff6332099ed9ceaa4889a6ce56a7a48817ddccc0952126059d07", url = "http://archive.ubuntu.com/ubuntu/pool/main/m/make-dfsg/make_4.1-9.1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_manpages", sha256 = "f937c30cf9b1474e699d0394ae1426d979be37f7443d3509c0e6bf22440e24d4", url = "http://archive.ubuntu.com/ubuntu/pool/main/m/manpages/manpages_4.15-1_all.deb")
    native.http_file(name = "ubuntu_manpages_dev", sha256 = "2ea36d0d91c2a649ea31ef382ce977775f297bbd9d80ba07606f4df7b3942a01", url = "http://archive.ubuntu.com/ubuntu/pool/main/m/manpages/manpages-dev_4.15-1_all.deb")
    native.http_file(name = "ubuntu_mime_support", sha256 = "98e05aa03538c5f182ed14cbb59cfe64b30592d77e602abd2442a9f1c72532b3", url = "http://archive.ubuntu.com/ubuntu/pool/main/m/mime-support/mime-support_3.60ubuntu1_all.deb")
    native.http_file(name = "ubuntu_multiarch_support", sha256 = "03709410ef65de9589e559318bb78e3ca3a86a43a92af7dd11d69e98f0a6067e", url = "http://archive.ubuntu.com/ubuntu/pool/main/g/glibc/multiarch-support_2.27-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_ncurses_term", sha256 = "cc6a69e2d32bcdc72c9fe45a228b4028fb16e987aa02465fd71db3db45ac8d21", url = "http://archive.ubuntu.com/ubuntu/pool/main/n/ncurses/ncurses-term_6.1-1ubuntu1.18.04_all.deb")
    native.http_file(name = "ubuntu_netbase", sha256 = "cbda1c8035cd1fe0b1fb09b456892c0bb868657bfe02da82f0b16207d391145e", url = "http://archive.ubuntu.com/ubuntu/pool/main/n/netbase/netbase_5.4_all.deb")
    native.http_file(name = "ubuntu_netcat_openbsd", sha256 = "aa5381260763cebb27e959718f5ff81540c9cee06b1d2ff8a34c9dcca1617596", url = "http://archive.ubuntu.com/ubuntu/pool/main/n/netcat-openbsd/netcat-openbsd_1.187-1ubuntu0.1_amd64.deb")
    native.http_file(name = "ubuntu_networkd_dispatcher", sha256 = "8e271a39b7573cdf9986f444ed7a65875f8ac046fe8cbffe0e439dd3ed93de0b", url = "http://archive.ubuntu.com/ubuntu/pool/main/n/networkd-dispatcher/networkd-dispatcher_1.7-0ubuntu3.2_all.deb")
    native.http_file(name = "ubuntu_openssh_client", sha256 = "2d7decfded66403931939338d610bf26ded453546208271f28f7b49d5a712e11", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openssh/openssh-client_7.6p1-4_amd64.deb")
    native.http_file(name = "ubuntu_openssh_server", sha256 = "39aa698589cb6637f2d6b97a96f47b73762d2b90204b047fdd91d09cb867f617", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openssh/openssh-server_7.6p1-4_amd64.deb")
    native.http_file(name = "ubuntu_openssh_sftp_server", sha256 = "97ad2696217c4069e9a96f95983da7326fdb21c7be487b5a225d378dd025ab3f", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openssh/openssh-sftp-server_7.6p1-4_amd64.deb")
    native.http_file(name = "ubuntu_openssl", sha256 = "76b504d89f7f2a83cb14caff40bff9a9b114c794cb6af8bc5f19dc87e34da6e0", url = "http://archive.ubuntu.com/ubuntu/pool/main/o/openssl/openssl_1.1.0g-2ubuntu4.1_amd64.deb")
    native.http_file(name = "ubuntu_patch", sha256 = "4300fec65dfb63b67049cbcbc206938ce41dbe986deafc5930189e09838ca1db", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/patch/patch_2.7.6-2ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_perl", sha256 = "95f67212e6883f330b1eab40cde2c51e767875fa678e9aa925d18c310b238f21", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/perl/perl_5.26.1-6ubuntu0.2_amd64.deb")
    native.http_file(name = "ubuntu_perl_modules_5_26", sha256 = "0913a1022863f546f07973261e63543d19c13725da52d44bc0a10449cf521c78", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/perl/perl-modules-5.26_5.26.1-6ubuntu0.2_all.deb")
    native.http_file(name = "ubuntu_pinentry_curses", sha256 = "18cd353bdf4ad53df6ea112d0477d1e4c1ceb3e140a6b083694f43d591ad9cad", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/pinentry/pinentry-curses_1.1.0-1_amd64.deb")
    native.http_file(name = "ubuntu_pkg_config", sha256 = "362e1f1573e97c5c2f5167721570fadfd5e23146472a293c002a62eca6417b07", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/pkg-config/pkg-config_0.29.1-0ubuntu2_amd64.deb")
    native.http_file(name = "ubuntu_publicsuffix", sha256 = "e7abd66c805a812e3c6b8d80f1221217cfb3f7675b5d260e579881d6cda0119d", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/publicsuffix/publicsuffix_20180223.1310-1_all.deb")
    native.http_file(name = "ubuntu_python2_7", sha256 = "3afa92b3c5813af7a8f4008d1bdfee1aea7d2da9135492f87a4c42ebae4a0f67", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python2.7/python2.7_2.7.15~rc1-1_amd64.deb")
    native.http_file(name = "ubuntu_python2_7_dev", sha256 = "4f366d3986a384485128f2b984214e486cb90325e1fe156856c98a303c9515d8", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python2.7/python2.7-dev_2.7.15~rc1-1_amd64.deb")
    native.http_file(name = "ubuntu_python2_7_minimal", sha256 = "018cf986dbd030a175bfc27cda1a98416a713d247abf5696e528314e8cfb948d", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python2.7/python2.7-minimal_2.7.15~rc1-1_amd64.deb")
    native.http_file(name = "ubuntu_python3", sha256 = "acd7f997890e8253a7ebad0ccde45a6ee88a5e7d7dd705b7f79b6e16d7d80790", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3-defaults/python3_3.6.5-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_python3_6", sha256 = "b2d99a6b1b29deb53c466ee851d38cdb5e641e0dfb8d96276ba161c4359fee6b", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3.6/python3.6_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_python3_6_dev", sha256 = "d6085530a5600eae97f296626e6dde95f0e3867c9a0aed7fa4cabca2c8636698", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3.6/python3.6-dev_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_python3_6_minimal", sha256 = "7525cb4edc3870c818d520d976dbb26cb96bdb446e6e88a842e77b7a920cd7bc", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3.6/python3.6-minimal_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_python3_6_venv", sha256 = "f9c4f2b8f80373eadbe1a2adfe9e29c805b70b939031cdb1c846a3b51b2bece0", url = "http://archive.ubuntu.com/ubuntu/pool/universe/p/python3.6/python3.6-venv_3.6.6-1~18.04_amd64.deb")
    native.http_file(name = "ubuntu_python3_certifi", sha256 = "bf58b7c4539ba282eda68d21755d1ef813403b0fbc46da6ec0107c8b24743e86", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python-certifi/python3-certifi_2018.1.18-2_all.deb")
    native.http_file(name = "ubuntu_python3_chardet", sha256 = "2c1ec04f4d71f00cc36217a44828aa14cdfd46f8017e4674b6181e84b2d34f80", url = "http://archive.ubuntu.com/ubuntu/pool/main/c/chardet/python3-chardet_3.0.4-1_all.deb")
    native.http_file(name = "ubuntu_python3_dbus", sha256 = "47f7bb0fe28baabb97982388bed7e8ebe4a9a59caa3c0184d47c52711f61f2e0", url = "http://archive.ubuntu.com/ubuntu/pool/main/d/dbus-python/python3-dbus_1.2.6-1_amd64.deb")
    native.http_file(name = "ubuntu_python3_dev", sha256 = "092733089c7e8714ad3ff47049e1153ca3b8b72b013a07c54b59fddd68cee531", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3-defaults/python3-dev_3.6.5-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_python3_distutils", sha256 = "29fb33ae1edb6da2cadb95506b0b99127d8b8405b2eb9f3e78dfaa4109fbc677", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3-stdlib-extensions/python3-distutils_3.6.5-3_all.deb")
    native.http_file(name = "ubuntu_python3_gi", sha256 = "f12891060d6851adc12b31147bd9b6379dc8d36ff2f9ebda383d1c2eebebad8f", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/pygobject/python3-gi_3.26.1-2_amd64.deb")
    native.http_file(name = "ubuntu_python3_idna", sha256 = "6cb85404887cd98bb633b6753bce833b37ff073562532da1762313ac51247962", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python-idna/python3-idna_2.6-1_all.deb")
    native.http_file(name = "ubuntu_python3_lib2to3", sha256 = "da61799a4268583ec7ef6cda2230e6b9df27d4455783f9fa708900c4ca02179a", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3-stdlib-extensions/python3-lib2to3_3.6.5-3_all.deb")
    native.http_file(name = "ubuntu_python3_minimal", sha256 = "9fdb6f981280b2ded7f7af8df08e507ae9c95bae2238ffadd6f0013b9c4da15f", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python3-defaults/python3-minimal_3.6.5-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_python3_pkg_resources", sha256 = "7720964ebebfb65b8689b4d25ceda8eda300f856bc81158e33af1ae19cd2a996", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python-setuptools/python3-pkg-resources_39.0.1-2_all.deb")
    native.http_file(name = "ubuntu_python3_requests", sha256 = "7bdf4213a239195f2da6e6d8032d2d44e8f43e21476f30493ee744d757e0e7c2", url = "http://archive.ubuntu.com/ubuntu/pool/main/r/requests/python3-requests_2.18.4-2ubuntu0.1_all.deb")
    native.http_file(name = "ubuntu_python3_setuptools", sha256 = "f342cdf67949f33faa0ecd3cef9d26dfb32ed0a7953b8cdebc2e0f92441065a7", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python-setuptools/python3-setuptools_39.0.1-2_all.deb")
    native.http_file(name = "ubuntu_python3_six", sha256 = "5c73f30a89f5906e84c66b38fcb0ea20b42c0376079beb6bcd35ba969befa6d7", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/six/python3-six_1.11.0-2_all.deb")
    native.http_file(name = "ubuntu_python3_urllib3", sha256 = "7c43ae04cc2dc94f66fd03ac1dddc028970c0333e2aa05c297ff17591ab840ca", url = "http://archive.ubuntu.com/ubuntu/pool/main/p/python-urllib3/python3-urllib3_1.22-1_all.deb")
    native.http_file(name = "ubuntu_python3_venv", sha256 = "39ccd9271a8bd11ef6a9b473ef5f0fef341114993686b0ad1c6673836f86bbea", url = "http://archive.ubuntu.com/ubuntu/pool/universe/p/python3-defaults/python3-venv_3.6.5-3ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_python_pip_whl", sha256 = "11c6a995e04659a2b8dbab6d86d275ef246cc6096f7ccc0be3a498f2ea3bb6a1", url = "http://archive.ubuntu.com/ubuntu/pool/universe/p/python-pip/python-pip-whl_9.0.1-2.3~ubuntu1_all.deb")
    native.http_file(name = "ubuntu_readline_common", sha256 = "84cb3642c82114496d2fc17011db13655bd661cf4641098c03c168ddde367908", url = "http://archive.ubuntu.com/ubuntu/pool/main/r/readline/readline-common_7.0-3_all.deb")
    native.http_file(name = "ubuntu_shared_mime_info", sha256 = "1278d560955970c818e52bd939dc5f7b6653cfe90a994bcd804ca5075802e5c0", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/shared-mime-info/shared-mime-info_1.9-2_amd64.deb")
    native.http_file(name = "ubuntu_ssh_import_id", sha256 = "c99ef603cda1d5ae4051a081cbff6a30402323683305df91853eb08de7d11092", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/ssh-import-id/ssh-import-id_5.7-0ubuntu1.1_all.deb")
    native.http_file(name = "ubuntu_systemd", sha256 = "89b9517d15b90e9c44a02fcd569b714b767a270a9929673e413f02194b8ea514", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/systemd/systemd_237-3ubuntu10.3_amd64.deb")
    native.http_file(name = "ubuntu_systemd_sysv", sha256 = "01e80f3bad96fa777807d83efb740fc5dc0220fd6f39cfa9c22eb173fa8b3d71", url = "http://archive.ubuntu.com/ubuntu/pool/main/s/systemd/systemd-sysv_237-3ubuntu10.3_amd64.deb")
    native.http_file(name = "ubuntu_ucf", sha256 = "fb069365dce07ec9b04110e806736c5a5d0b0f350216c90912749226f1187ed5", url = "http://archive.ubuntu.com/ubuntu/pool/main/u/ucf/ucf_3.0038_all.deb")
    native.http_file(name = "ubuntu_unzip", sha256 = "d46069c369ce88c8dd91c52abb8de8d6053606748ef18b3b9bc290fdd8ad2953", url = "http://archive.ubuntu.com/ubuntu/pool/main/u/unzip/unzip_6.0-21ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_wget", sha256 = "d57eda480259e38d57310e2c98badc991d318ba9725853fc8e6d063464e44308", url = "http://archive.ubuntu.com/ubuntu/pool/main/w/wget/wget_1.19.4-1ubuntu2.1_amd64.deb")
    native.http_file(name = "ubuntu_xauth", sha256 = "f499832a25338aacc727be300d182a668951923824958898ff1985e046d6a352", url = "http://archive.ubuntu.com/ubuntu/pool/main/x/xauth/xauth_1.0.10-1_amd64.deb")
    native.http_file(name = "ubuntu_xdg_user_dirs", sha256 = "68ddeef702701e78bc22a19c935839faa99dff56e7245b9f6e26d754eac2b887", url = "http://archive.ubuntu.com/ubuntu/pool/main/x/xdg-user-dirs/xdg-user-dirs_0.17-1ubuntu1_amd64.deb")
    native.http_file(name = "ubuntu_xz_utils", sha256 = "0c142fd5c44cae76e2e1ad5c62d3dc8f36f1a80eaaadf9a5e60325fcfffe16ed", url = "http://archive.ubuntu.com/ubuntu/pool/main/x/xz-utils/xz-utils_5.2.2-1.3_amd64.deb")
    native.http_file(name = "ubuntu_zip", sha256 = "800a02ee081bc8cedafe6b0a3da1193de3e0c281ae1e7a04b091db01cd301d92", url = "http://archive.ubuntu.com/ubuntu/pool/main/z/zip/zip_3.0-11build1_amd64.deb")
    native.http_file(name = "ubuntu_zlib1g_dev", sha256 = "1bd6bfc66d1de113f14a9afdd61d7f4b911c11c570403dd9785aa937b88f9ea9", url = "http://archive.ubuntu.com/ubuntu/pool/main/z/zlib/zlib1g-dev_1.2.11.dfsg-0ubuntu2_amd64.deb")

def deb_files():
    return [
        "@ubuntu_binutils//file",
        "@ubuntu_binutils_common//file",
        "@ubuntu_binutils_x86_64_linux_gnu//file",
        "@ubuntu_build_essential//file",
        "@ubuntu_ca_certificates//file",
        "@ubuntu_cpp//file",
        "@ubuntu_cpp_7//file",
        "@ubuntu_curl//file",
        "@ubuntu_dbus//file",
        "@ubuntu_dh_python//file",
        "@ubuntu_dirmngr//file",
        "@ubuntu_dmsetup//file",
        "@ubuntu_dpkg_dev//file",
        "@ubuntu_fakeroot//file",
        "@ubuntu_file//file",
        "@ubuntu_gPP//file",
        "@ubuntu_gPP_7//file",
        "@ubuntu_gcc//file",
        "@ubuntu_gcc_7//file",
        "@ubuntu_gcc_7_base//file",
        "@ubuntu_gir1_2_glib_2_0//file",
        "@ubuntu_git//file",
        "@ubuntu_git_man//file",
        "@ubuntu_gnupg//file",
        "@ubuntu_gnupg_l10n//file",
        "@ubuntu_gnupg_utils//file",
        "@ubuntu_gpg//file",
        "@ubuntu_gpg_agent//file",
        "@ubuntu_gpg_wks_client//file",
        "@ubuntu_gpg_wks_server//file",
        "@ubuntu_gpgconf//file",
        "@ubuntu_gpgsm//file",
        "@ubuntu_krb5_locales//file",
        "@ubuntu_less//file",
        "@ubuntu_libalgorithm_diff_perl//file",
        "@ubuntu_libalgorithm_diff_xs_perl//file",
        "@ubuntu_libalgorithm_merge_perl//file",
        "@ubuntu_libapparmor1//file",
        "@ubuntu_libargon2_0//file",
        "@ubuntu_libasan4//file",
        "@ubuntu_libasn1_8_heimdal//file",
        "@ubuntu_libassuan0//file",
        "@ubuntu_libatomic1//file",
        "@ubuntu_libbinutils//file",
        "@ubuntu_libbsd0//file",
        "@ubuntu_libc6_dev//file",
        "@ubuntu_libc_dev_bin//file",
        "@ubuntu_libcap2//file",
        "@ubuntu_libcc1_0//file",
        "@ubuntu_libcilkrts5//file",
        "@ubuntu_libcryptsetup12//file",
        "@ubuntu_libcurl3_gnutls//file",
        "@ubuntu_libcurl4//file",
        "@ubuntu_libdbus_1_3//file",
        "@ubuntu_libdevmapper1_02_1//file",
        "@ubuntu_libdpkg_perl//file",
        "@ubuntu_libedit2//file",
        "@ubuntu_liberror_perl//file",
        "@ubuntu_libexpat1//file",
        "@ubuntu_libexpat1_dev//file",
        "@ubuntu_libfakeroot//file",
        "@ubuntu_libfile_fcntllock_perl//file",
        "@ubuntu_libgcc_7_dev//file",
        "@ubuntu_libgdbm5//file",
        "@ubuntu_libgdbm_compat4//file",
        "@ubuntu_libgirepository_1_0_1//file",
        "@ubuntu_libglib2_0_0//file",
        "@ubuntu_libglib2_0_data//file",
        "@ubuntu_libgomp1//file",
        "@ubuntu_libgssapi3_heimdal//file",
        "@ubuntu_libgssapi_krb5_2//file",
        "@ubuntu_libhcrypto4_heimdal//file",
        "@ubuntu_libheimbase1_heimdal//file",
        "@ubuntu_libheimntlm0_heimdal//file",
        "@ubuntu_libhx509_5_heimdal//file",
        "@ubuntu_libicu60//file",
        "@ubuntu_libidn11//file",
        "@ubuntu_libip4tc0//file",
        "@ubuntu_libisl19//file",
        "@ubuntu_libitm1//file",
        "@ubuntu_libjson_c3//file",
        "@ubuntu_libk5crypto3//file",
        "@ubuntu_libkeyutils1//file",
        "@ubuntu_libkmod2//file",
        "@ubuntu_libkrb5_26_heimdal//file",
        "@ubuntu_libkrb5_3//file",
        "@ubuntu_libkrb5support0//file",
        "@ubuntu_libksba8//file",
        "@ubuntu_libldap_2_4_2//file",
        "@ubuntu_libldap_common//file",
        "@ubuntu_liblocale_gettext_perl//file",
        "@ubuntu_liblsan0//file",
        "@ubuntu_libltdl7//file",
        "@ubuntu_libmagic1//file",
        "@ubuntu_libmagic_mgc//file",
        "@ubuntu_libmpc3//file",
        "@ubuntu_libmpdec2//file",
        "@ubuntu_libmpfr6//file",
        "@ubuntu_libmpx2//file",
        "@ubuntu_libnghttp2_14//file",
        "@ubuntu_libnpth0//file",
        "@ubuntu_libnss_systemd//file",
        "@ubuntu_libpam_systemd//file",
        "@ubuntu_libperl5_26//file",
        "@ubuntu_libpsl5//file",
        "@ubuntu_libpython2_7//file",
        "@ubuntu_libpython2_7_dev//file",
        "@ubuntu_libpython2_7_minimal//file",
        "@ubuntu_libpython2_7_stdlib//file",
        "@ubuntu_libpython3_6//file",
        "@ubuntu_libpython3_6_dev//file",
        "@ubuntu_libpython3_6_minimal//file",
        "@ubuntu_libpython3_6_stdlib//file",
        "@ubuntu_libpython3_dev//file",
        "@ubuntu_libpython3_stdlib//file",
        "@ubuntu_libquadmath0//file",
        "@ubuntu_libreadline7//file",
        "@ubuntu_libroken18_heimdal//file",
        "@ubuntu_librtmp1//file",
        "@ubuntu_libsasl2_2//file",
        "@ubuntu_libsasl2_modules//file",
        "@ubuntu_libsasl2_modules_db//file",
        "@ubuntu_libsqlite3_0//file",
        "@ubuntu_libssl1_0_0//file",
        "@ubuntu_libssl1_1//file",
        "@ubuntu_libstdcPP_7_dev//file",
        "@ubuntu_libtsan0//file",
        "@ubuntu_libubsan0//file",
        "@ubuntu_libwind0_heimdal//file",
        "@ubuntu_libwrap0//file",
        "@ubuntu_libx11_6//file",
        "@ubuntu_libx11_data//file",
        "@ubuntu_libxau6//file",
        "@ubuntu_libxcb1//file",
        "@ubuntu_libxdmcp6//file",
        "@ubuntu_libxext6//file",
        "@ubuntu_libxml2//file",
        "@ubuntu_libxmuu1//file",
        "@ubuntu_linux_libc_dev//file",
        "@ubuntu_make//file",
        "@ubuntu_manpages//file",
        "@ubuntu_manpages_dev//file",
        "@ubuntu_mime_support//file",
        "@ubuntu_multiarch_support//file",
        "@ubuntu_ncurses_term//file",
        "@ubuntu_netbase//file",
        "@ubuntu_netcat_openbsd//file",
        "@ubuntu_networkd_dispatcher//file",
        "@ubuntu_openssh_client//file",
        "@ubuntu_openssh_server//file",
        "@ubuntu_openssh_sftp_server//file",
        "@ubuntu_openssl//file",
        "@ubuntu_patch//file",
        "@ubuntu_perl//file",
        "@ubuntu_perl_modules_5_26//file",
        "@ubuntu_pinentry_curses//file",
        "@ubuntu_pkg_config//file",
        "@ubuntu_publicsuffix//file",
        "@ubuntu_python2_7//file",
        "@ubuntu_python2_7_dev//file",
        "@ubuntu_python2_7_minimal//file",
        "@ubuntu_python3//file",
        "@ubuntu_python3_6//file",
        "@ubuntu_python3_6_dev//file",
        "@ubuntu_python3_6_minimal//file",
        "@ubuntu_python3_6_venv//file",
        "@ubuntu_python3_certifi//file",
        "@ubuntu_python3_chardet//file",
        "@ubuntu_python3_dbus//file",
        "@ubuntu_python3_dev//file",
        "@ubuntu_python3_distutils//file",
        "@ubuntu_python3_gi//file",
        "@ubuntu_python3_idna//file",
        "@ubuntu_python3_lib2to3//file",
        "@ubuntu_python3_minimal//file",
        "@ubuntu_python3_pkg_resources//file",
        "@ubuntu_python3_requests//file",
        "@ubuntu_python3_setuptools//file",
        "@ubuntu_python3_six//file",
        "@ubuntu_python3_urllib3//file",
        "@ubuntu_python3_venv//file",
        "@ubuntu_python_pip_whl//file",
        "@ubuntu_readline_common//file",
        "@ubuntu_shared_mime_info//file",
        "@ubuntu_ssh_import_id//file",
        "@ubuntu_systemd//file",
        "@ubuntu_systemd_sysv//file",
        "@ubuntu_ucf//file",
        "@ubuntu_unzip//file",
        "@ubuntu_wget//file",
        "@ubuntu_xauth//file",
        "@ubuntu_xdg_user_dirs//file",
        "@ubuntu_xz_utils//file",
        "@ubuntu_zip//file",
        "@ubuntu_zlib1g_dev//file",
    ]
