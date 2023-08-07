FROM gitpod/workspace-base

RUN sudo apt-get update
RUN sudo curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
RUN ghcup install ghc 9.2.5 && ghcup install cabal 3.6.2.0
RUN ghcup set ghc 9.2.5 && ghcup set cabal 3.6.2.0