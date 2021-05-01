let Greeter = (function () {
  let privateProps = new WeakMap();

  function getLogin() {
    return privateProps.get(this).login;
  }

  function getPassword() {
    return privateProps.get(this).password;
  }

  class Greeter {
    constructor() {
      privateProps.set(this, {
        login: "123@xyz.com",
        password: "qwertys123321"
      });
    }

    getUserData(login, password) {
      if (login === "123@xyz.com" && password === "qwertys123321") {
        return {
          accessToken: "AOMSIPFD9jEA)FsnpdF",
          refresh: 300,
          username: "Vasyan3000"
        };
      }

      return null;
    }

    greet() {
      let login = getLogin.call(this);
      let password = getPassword.call(this);

      let userData = this.getUserData(login, password);
      console.log(`Hello, ${userData.username}!`);
    }
  }

  return Greeter;
})();

let greeter = new Greeter();

greeter.greet(); // Hello, Vasyan3000!
greeter.getUserData(); // null
greeter.getLogin(); // TypeError: greeter.getLogin is not a function
greeter.getPassword(); // TypeError: greeter.getPassword is not a function
greeter.privateProps; // undefined
