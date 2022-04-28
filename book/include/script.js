<script>
$( document ).ready(function() {
  var cite = ' ';
  var license = ' <a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/" target="blank"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png"></a>';

  $("footer div.row div:eq(1) p").html(
    license + cite
  );
  
  $(".checklist li").prepend('<input type="checkbox" /> ');

  // open rdrr links externally
  $("a[href^='https://rdrr.io']").click(function(){
    window.open(this.href);
    return false;
  });

  // visible second sidebar in mobile
  function move_sidebar() {
    var w = window.innerWidth;
    if (w < 992) {
      $("#toc").appendTo($("#main-nav"));
    } else {
      $("#toc").appendTo($("div.sidebar-chapter"));
    }
  }
  move_sidebar();
  window.onresize = move_sidebar;
});
</script>
