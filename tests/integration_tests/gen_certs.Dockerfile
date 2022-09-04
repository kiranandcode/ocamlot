FROM debian:11

RUN apt update
RUN apt install -y openssl

WORKDIR /certs

RUN echo $"rm -f ./openssl.cnf\n\
touch ./openssl.cnf \n\
{ \
    cat /etc/ssl/openssl.cnf; \n\
    echo \"\"; \n\
    echo \"[ SAN ]\"; \n\
    echo \"subjectAltName=DNS.1:pleroma.ocamlot.xyz,DNS.2:testing.ocamlot.xyz\"; \n\
} >> ./openssl.cnf \n\
\n\
openssl req -x509 -sha256 -nodes -newkey rsa:4096 -keyout ocamlot.key -out ocamlot.crt \
        -subj \"/CN=*.ocamlot.xyz/\" -config ./openssl.cnf -extensions SAN" > ./gen_certs.sh

RUN chmod u+x ./gen_certs.sh
RUN ./gen_certs.sh
