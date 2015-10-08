{% include "header.tpl" %}

<h3 style="text-align: center;">{{ uname }}'s corner.</h3>

<p><em>Joined on: {{ joined_on }}</em></p>

{% for song in songs %}

<li><strong>{{ song.value.songTitle }}</strong>
  <audio controls>
    <source src="/songs/{{ song.value.songFilepath }}" type="audio/mpeg">
    Your browser is not supported
  </audio>
</li>

{% else %}

No song yet.

{% endfor %}

{% if itsMe %}

<p>This is your profile. <a href="/u/upload">Upload a song</a>.</p>

{% endif %}

{% include "footer.tpl" %}
