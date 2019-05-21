# ProofHub

This is ProofHub project.

### TODO
- [x] Search projects
- [x] Pagination
- [x] Update user info
- [x] Categories and authors when adding and updating
- [ ] Test everything
- [ ] Add Bootstrap and make fancy
- [ ] Add syntax highlight
- [ ] Add Gentzen view for coq (Optional)

### Installation

* `git clone https://github.com/zelinskiy/ProofHub`
* `cd ProofHub/api`
* `stack build`

* `cd ProofHub/front`
* `elm make`
* `elm reactor &`

### Postgres setup
```
sudo apt-get install postgresql-10 libpq-dev postgresql-client-10
sudo -u postgres createuser -se test
sudo -u postgres psql -c "alter role test with password 'test'"
sudo -u postgres psql -c "create database test"
psql -U test -d test
```

### Curl stuff
```
curl --header "Content-Type: application/json" --request POST --data '{"email":"admin","pass":"administrator1"}' http://localhost:8080/public/user/register
curl -v --output kek.txt --header "Content-Type: application/json" --request POST --data '{"email":"admin","pass":"administrator1"}' http://localhost:8080/public/jwt/login
curl -v --header "Authorization: Bearer $(cat kek.txt | tr -d '"')" http://localhost:8080/private/user/me
```
