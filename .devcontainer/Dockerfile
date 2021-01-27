FROM alpine
RUN apk add --update zsh git make m4 build-base
RUN sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
SHELL ["/bin/zsh", "-c"]
RUN apk add opam
RUN opam init -vv -a -c 4.10.1 --disable-sandboxing
RUN echo "source ~/.profile" >> ~/.zshrc
RUN source ~/.zshrc
# Installing utop will also install ocamlbuild. For this small project,
# ocamlbuild should be enough.
RUN opam install --yes utop ounit ocaml-lsp-server ocamlformat
