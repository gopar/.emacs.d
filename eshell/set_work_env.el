;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color
;;
;; This script is to be ran after you were able to follow the guide at
;; https://github.com/color/color/wiki/Setup-Development-Environment
;;
;; Reason for this is to make sure that if you run into an error, we can more
;; safely assume it has to be related to your emacs config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "COLOR_ROOT" "${HOME}/work/color" t)

;; Find where asdf dir is located in:
;; $ echo $(brew --prefix asdf)
(setenv "ASDF_DIR" "/usr/local/opt/asdf/libexec")

;; Default aws config.
(setenv "AWS_DEFAULT_REGION" "us-east-1")
(setenv "AWS_PROFILE" "color-dev-eng")

;; For clrenv
(setenv "COLOR_KEY_MODE" "dev")
(setenv "CLRYPT_CERT" "${COLOR_ROOT}/.ec2/${COLOR_KEY_MODE}.crt" t)
(setenv "CLRYPT_PK" "${HOME}/.ec2/${COLOR_KEY_MODE}.pem" t)
(setenv "CLRENV_MODE" "${COLOR_KEY_MODE}-keys" t)
(setenv "CLRENV_OVERLAY_PATH" "${COLOR_ROOT}/environment.local.yaml:${COLOR_ROOT}/local/environment.user.yaml" t)


;; See src/infra/docker/color-base-python/entrypoint.sh
(setenv "COLOR_LOCAL_DEV" "1")

;; Set pointers to the ec2 certificates
(setenv "COLOR_EC2_CERT" "${HOME}/.ec2/$COLOR_KEY_MODE.crt" t)
(setenv "COLOR_EC2_PK" "${HOME}/.ec2/$COLOR_KEY_MODE.pem" t)

;; For clrenv
(setenv "CLRENV_PATH" "${COLOR_ROOT}/environment.yaml" t)
(setenv "CLRENV_OVERLAY_PATH" "${COLOR_ROOT}/environment.local.yaml:${COLOR_ROOT}/local/environment.user.yaml" t)
(setenv "CLRENV_MODE" "${COLOR_KEY_MODE}-keys" t)

;; For clrypt
(setenv "CLRYPT_CERT" "${COLOR_EC2_CERT}" t)
(setenv "CLRYPT_PK" "${COLOR_EC2_PK}" t)
(setenv "ENCRYPTED_DIR" "${COLOR_ROOT}/encrypted" t)

;; Ensure consistent language
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LANGUAGE" "en_US.UTF-8")

;; Paths
(setenv "PATH" "${COLOR_ROOT}/src/bin/:${PATH}" t)
(setenv "PATH" "${HOME}/.asdf/shims/:${ASDF_DIR}/bin/:${PATH}" t)

;; Misc
(setenv "PGDATA" "${COLOR_ROOT}/local/pgsql/data" t)
(setenv "PYTHONPATH" "${COLOR_ROOT}/src:${PYTHONPATH}" t)
(setenv "NODE_MODULES_PATH" "${COLOR_ROOT}/src/django_web/node_modules" t)

;; Datadog related
(setenv "DD_TRACE_ENABLED" "false")

;; Required to avoid high CPU usage in node.js hot reload file watch code on MacOS
;; See https://www.typescriptlang.org/docs/handbook/configuring-watch.html
(setenv "TSC_WATCHFILE" "UseFsEvents")
