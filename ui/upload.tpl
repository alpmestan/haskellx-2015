{% include "header.tpl" %}

<h3 style="text-align: center;">Upload a song</h3>

<form action="/u/upload" method="post" enctype="multipart/form-data">
  <input name="song_title" placeholder="Song title" required />
  <input name="song_file" accept="audio/*" type="file" placeholder="Upload a file" required /> (only OGG and MP3 supported by most browsers)
  <input type="submit" value="Upload" />
</form>

{% include "footer.tpl" %}
