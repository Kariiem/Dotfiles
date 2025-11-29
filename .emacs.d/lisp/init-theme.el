;; -*- lexical-binding: t -*-
(install-pkgs gruber-darker-theme
              dracula-theme
              ef-themes
              modus-themes
              ;; doom themes
              doom-themes
              fantom-theme
              ;; uncoloured themes
              tao-theme)

(defvar after-load-theme-hook nil)

(advice-add #'load-theme :before (lambda (&rest _) (mapc 'disable-theme custom-enabled-themes)))
(advice-add #'load-theme :after (lambda (&rest _) (run-hooks 'after-load-theme-hook)))

(custom-set-faces '(info-menu-header ((t (:family "Jetbrains Mono" :weight bold)))))

(add-hook 'after-load-theme-hook (lambda () (set-face-attribute 'region nil :extend t)))


(with-eval-after-load 'notmuch
  (defun set-notmuch-faces ()
    (face-remap-add-relative 'notmuch-tag-face :foreground "blue")
    (face-remap-add-relative 'hl-line :background "#00ffff")
    (face-remap-add-relative 'default :foreground "#000000" :background "#f6f2f6")
    (face-remap-add-relative 'widget-button :foreground "#2376a5"))

  (defun toggle-notmuch-theme ()
    (interactive)
    (add-hook 'notmuch-hello-mode-hook 'set-notmuch-faces)
    (add-hook 'notmuch-search-mode-hook 'set-notmuch-faces)
    (add-hook 'notmuch-message-mode-hook 'set-notmuch-faces)
    (add-hook 'notmuch-show-mode-hook 'set-notmuch-faces)))


(custom-set-variables
 '(custom-safe-themes
   (append '(default)
           (mapcar
            (lambda (filename)
              (with-temp-buffer
                (insert-file-contents filename)
                (secure-hash 'sha256 (buffer-string))))
            (file-expand-wildcards (concat user-emacs-directory
                                           "elpa/*/*-theme.el"))))))

;; this list was evaluated by running the above expression in ielm
;; (custom-set-variables
;;  '(custom-safe-themes
;;    '(default "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
;;              "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
;;              "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
;;              "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
;;              "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
;;              "5244ba0273a952a536e07abaad1fdf7c90d7ebb3647f36269c23bfd1cf20b0b8"
;;              "83550d0386203f010fa42ad1af064a766cfec06fc2f42eb4f2d89ab646f3ac01"
;;              "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
;;              "22a0d47fe2e6159e2f15449fcb90bbf2fe1940b185ff143995cc604ead1ea171"
;;              "599f72b66933ea8ba6fce3ae9e5e0b4e00311c2cbf01a6f46ac789227803dd96"
;;              "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
;;              "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
;;              "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
;;              "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
;;              "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
;;              "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
;;              "c3c135e69890de6a85ebf791017d458d3deb3954f81dcb7ac8c430e1620bb0f1"
;;              "4b88b7ca61eb48bb22e2a4b589be66ba31ba805860db9ed51b4c484f3ef612a7"
;;              "e1df746a4fa8ab920aafb96c39cd0ab0f1bac558eff34532f453bd32c687b9d6"
;;              "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
;;              "b7a09eb77a1e9b98cafba8ef1bd58871f91958538f6671b22976ea38c2580755"
;;              "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
;;              "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
;;              "6963de2ec3f8313bb95505f96bf0cf2025e7b07cefdb93e3d2e348720d401425"
;;              "2f8af2a3a2fae6b6ea254e7aab6f3a8b5c936428b67869cef647c5f8e7985877"
;;              "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
;;              "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
;;              "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
;;              "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
;;              "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
;;              "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
;;              "088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
;;              "1f8bd4db8280d5e7c5e6a12786685a7e0c6733b0e3cf99f839fb211236fb4529"
;;              "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22"
;;              "3f24dd8f542f4aa8186a41d5770eb383f446d7228cd7a3413b9f5e0ec0d5f3c0"
;;              "4d714a034e7747598869bef1104e96336a71c3d141fa58618e4606a27507db4c"
;;              "72d9086e9e67a3e0e0e6ba26a1068b8b196e58a13ccaeff4bfe5ee6288175432"
;;              "a368631abdadffb6882f9994637d7216167912311447f1ec02f9dc58e9cc62a9"
;;              "19d62171e83f2d4d6f7c31fc0a6f437e8cec4543234f0548bad5d49be8e344cd"
;;              "e4d4cc443964b8a64defc06de3edb2363f7cb1b3c3ae2272b2c1487f626e4318"
;;              "1bc640af8b000ae0275dbffefa2eb22ec91f6de53aca87221c125dc710057511"
;;              "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
;;              "42a6583a45e0f413e3197907aa5acca3293ef33b4d3b388f54fa44435a494739"
;;              "87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
;;              "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
;;              "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
;;              "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1"
;;              "2f7fa7a92119d9ed63703d12723937e8ba87b6f3876c33d237619ccbd60c96b9"
;;              "f253a920e076213277eb4cbbdf3ef2062e018016018a941df6931b995c6ff6f6"
;;              "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
;;              "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
;;              "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
;;              "2ab8cb6d21d3aa5b821fa638c118892049796d693d1e6cd88cb0d3d7c3ed07fc"
;;              "d12b1d9b0498280f60e5ec92e5ecec4b5db5370d05e787bc7cc49eae6fb07bc0"
;;              "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
;;              "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
;;              "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079"
;;              "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
;;              "921f165deb8030167d44eaa82e85fcef0254b212439b550a9b6c924f281b5695"
;;              "e14289199861a5db890065fdc5f3d3c22c5bac607e0dbce7f35ce60e6b55fc52"
;;              "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
;;              "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
;;              "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
;;              "21d2bf8d4d1df4859ff94422b5e41f6f2eeff14dd12f01428fa3cb4cb50ea0fb"
;;              "d97ac0baa0b67be4f7523795621ea5096939a47e8b46378f79e78846e0e4ad3d"
;;              "0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
;;              "0d2c5679b6d087686dcfd4d7e57ed8e8aedcccc7f1a478cd69704c02e4ee36fe"
;;              "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
;;              "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
;;              "5c8a1b64431e03387348270f50470f64e28dfae0084d33108c33a81c1e126ad6"
;;              "7771c8496c10162220af0ca7b7e61459cb42d18c35ce272a63461c0fc1336015"
;;              "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
;;              "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
;;              "7c3d62a64bafb2cc95cd2de70f7e4446de85e40098ad314ba2291fc07501b70c"
;;              "b99ff6bfa13f0273ff8d0d0fd17cc44fab71dfdc293c7a8528280e690f084ef0"
;;              "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
;;              "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
;;              "2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7"
;;              "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac"
;;              "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729"
;;              "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
;;              "a3a71b922fb6cbf9283884ac8a9109935e04550bcc5d2a05414a58c52a8ffc47"
;;              "cd5f8f91cc2560c017cc9ec24a9ab637451e36afd22e00a03e08d7b1b87c29ca"
;;              "1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac"
;;              "ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
;;              "211621592803ada9c81ec8f8ba0659df185f9dc06183fcd0e40fbf646c995f23"
;;              "296dcaeb2582e7f759e813407ff1facfd979faa071cf27ef54100202c45ae7d4"
;;              "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
;;              "d6b369a3f09f34cdbaed93eeefcc6a0e05e135d187252e01b0031559b1671e97"
;;              "df39cc8ecf022613fc2515bccde55df40cb604d7568cb96cd7fe1eff806b863b"
;;              "e85a354f77ae6c2e47667370a8beddf02e8772a02e1f7edb7089e793f4762a45"
;;              "d609d9aaf89d935677b04d34e4449ba3f8bbfdcaaeeaab3d21ee035f43321ff1"
;;              "b1791a921c4f38cb966c6f78633364ad880ad9cf36eef01c60982c54ec9dd088"
;;              "ac893acecb0f1cf2b6ccea5c70ea97516c13c2b80c07f3292c21d6eb0cb45239"
;;              "6af300029805f10970ebec4cea3134f381cd02f04c96acba083c76e2da23f3ec"
;;              "c038d994d271ebf2d50fa76db7ed0f288f17b9ad01b425efec09519fa873af53"
;;              "aff0396925324838889f011fd3f5a0b91652b88f5fd0611f7b10021cc76f9e09"
;;              "4c16a8be2f20a68f0b63979722676a176c4f77e2216cc8fe0ea200f597ceb22e"
;;              "90185f1d8362727f2aeac7a3d67d3aec789f55c10bb47dada4eefb2e14aa5d01"
;;              "ffa78fc746f85d1c88a2d1691b1e37d21832e9a44a0eeee114a00816eabcdaf9"
;;              "cee5c56dc8b95b345bfe1c88d82d48f89e0f23008b0c2154ef452b2ce348da37"
;;              "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2"
;;              "19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf"
;;              "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24"
;;              "b3ba955a30f22fe444831d7bc89f6466b23db8ce87530076d1f1c30505a4c23b"
;;              "71b688e7ef7c844512fa7c4de7e99e623de99a2a8b3ac3df4d02f2cd2c3215e7"
;;              "3d9938bbef24ecee9f2632cb25339bf2312d062b398f0dfb99b918f8f11e11b1"
;;              "541282f66e5cc83918994002667d2268f0a563205117860e71b7cb823c1a11e9"
;;              "da69584c7fe6c0acadd7d4ce3314d5da8c2a85c5c9d0867c67f7924d413f4436"
;;              "a0e9bc5696ce581f09f7f3e7228b949988d76da5a8376e1f2da39d1d026af386"
;;              "2551f2b4bc12993e9b8560144fb072b785d4cddbef2b6ec880c602839227b8c7"
;;              "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a"
;;              "c53db9aec64c633360ecb6c1200fee65b55c528ba5dc3853c9c357024b5296c4"
;;              "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
;;              "8113b541640681ac92f5c74b087111a0865fd491431a823efddba43951ff7f58"
;;              "0592f1b03ba5d7d8cfade2ce3a23db4c0d5f9926c9ae918dd740739dd95b697c"
;;              "9113a2a0e6f13b8fe851c6c5a9b2a1a9608b9aae28b411c81211315b2e312007"
;;              "671683b4bd5f3b83f9db6cffe1d34a5707520c4a9c4ff0ff24bb24d1f316bc7b"
;;              "3187b36b1ed26ac57a79861baca06de24c0232dcb3e35c46931e45dd3c031dce"
;;              "292a7482026054ebf039036f5f0a8cb670dea0c76bb8d34b6c9d74e19db8a9bc"
;;              "e36b78ef2b29a76c8487061af440de56e2b8481e6c9ef8cdc2a72cfd9d2475d2"
;;              "6352b7fab474438433ed2b1d82eff40379aca9cfa4495bf1d098a91706af485c"
;;              "aa545934ce1b6fd16b4db2cf6c2ccf126249a66712786dd70f880806a187ac0b"
;;              "0b41a4a9f81967daacd737f83d3eac7e3112d642e3f786cf7613de4da97a830a"
;;              "bf4d25079f7d052cb656e099d9c2af9fb61ee377e8e72b7f13cecf8dffb74f92"
;;              "a372fd35724ebb25694e8f977fde62af3e9dd5e31d71005968545042419fa47d"
;;              "8896994441276c7ae32463776d5cbb71b51d8e3241b1b7c981306a3c4fbead07"
;;              "4d66d185c52e429e814f98265ee34b314bf0ea21a9c0bd020ef406e9f37c15a6"
;;              "acfe7ff6aacb9432f124cde4e35d6d2b4bc52916411de73a6ccded9750c9fa97"
;;              "b8bd60a23b9e2f08b0c437231ee84f2dacc70fdc4d5a0fb87229bb9926273fdd"
;;              "227dd4519dd40777533a58f905dccb9ca61928aef9f52761c8194e2e8bbb5f8b"
;;              "d4e3c3d8ce6d5373e102d9412da578cf768da43ab19fbd46bb6103b48294acf3"
;;              "d35afe834d1f808c2d5dc7137427832ccf99ad2d3d65d65f35cc5688404fdf30")))

(provide 'init-theme)
