{% include "header.tpl" %}

<form action="/login" method="post">
  <input type="text" name="username" placeholder="Username" required />
  <input type="password" name="password" placeholder="Password" required />
  <input type="submit" value="&gt;&gt;=" />
</form>

{% include "footer.tpl" %}
