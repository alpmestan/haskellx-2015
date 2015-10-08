{% include "header.tpl" %}

<h3 style="text-align: center;">Register</h3>

<form action="/register" method="post">
  <input type="text" name="username" placeholder="Username" required />
  <input type="password" name="password" placeholder="Password" required />
  <input type="submit" value="&gt;&gt;=" />
</form>

{% include "footer.tpl" %}
