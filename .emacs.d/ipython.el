  


<!DOCTYPE html>
<html>
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# githubog: http://ogp.me/ns/fb/githubog#">
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <title>ipython/docs/emacs/ipython.el at master · ipython/ipython · GitHub</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub" />
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub" />
    <link rel="apple-touch-icon" sizes="57x57" href="/apple-touch-icon-114.png" />
    <link rel="apple-touch-icon" sizes="114x114" href="/apple-touch-icon-114.png" />
    <link rel="apple-touch-icon" sizes="72x72" href="/apple-touch-icon-144.png" />
    <link rel="apple-touch-icon" sizes="144x144" href="/apple-touch-icon-144.png" />
    <link rel="logo" type="image/svg" href="http://github-media-downloads.s3.amazonaws.com/github-logo.svg" />
    <link rel="xhr-socket" href="/_sockets">
    <meta name="msapplication-TileImage" content="/windows-tile.png">
    <meta name="msapplication-TileColor" content="#ffffff">

    
    
    <link rel="icon" type="image/x-icon" href="/favicon.ico" />

    <meta content="authenticity_token" name="csrf-param" />
<meta content="L0ISweYmxNfk+U8dcvlMM1OnRsNhARbEavFOIN6gPI4=" name="csrf-token" />

    <link href="https://a248.e.akamai.net/assets.github.com/assets/github-21d1f919c9b16786238504ad1232b4937bbdd088.css" media="all" rel="stylesheet" type="text/css" />
    <link href="https://a248.e.akamai.net/assets.github.com/assets/github2-b702f2bd3370655be3cd89c1cd8cb00052d6a475.css" media="all" rel="stylesheet" type="text/css" />
    


      <script src="https://a248.e.akamai.net/assets.github.com/assets/frameworks-92d138f450f2960501e28397a2f63b0f100590f0.js" type="text/javascript"></script>
      <script src="https://a248.e.akamai.net/assets.github.com/assets/github-8b597885ade82d6a3d9108d93ea8e86b9f294d91.js" type="text/javascript"></script>
      
      <meta http-equiv="x-pjax-version" content="77a530946004f4df817ad3a299a913ed">

        <link data-pjax-transient rel='permalink' href='/ipython/ipython/blob/4d76f67a84c0b184c855f234accf5de7d697afab/docs/emacs/ipython.el'>
    <meta property="og:title" content="ipython"/>
    <meta property="og:type" content="githubog:gitrepository"/>
    <meta property="og:url" content="https://github.com/ipython/ipython"/>
    <meta property="og:image" content="https://secure.gravatar.com/avatar/f72497397dd9a0a79c654c8182460bb1?s=420&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png"/>
    <meta property="og:site_name" content="GitHub"/>
    <meta property="og:description" content="ipython - Official repository for IPython itself. Other repos in the IPython organization contain things like the website, documentation builds, etc."/>
    <meta property="twitter:card" content="summary"/>
    <meta property="twitter:site" content="@GitHub">
    <meta property="twitter:title" content="ipython/ipython"/>

    <meta name="description" content="ipython - Official repository for IPython itself. Other repos in the IPython organization contain things like the website, documentation builds, etc." />

  <link href="https://github.com/ipython/ipython/commits/master.atom" rel="alternate" title="Recent Commits to ipython:master" type="application/atom+xml" />

  </head>


  <body class="logged_out page-blob  vis-public env-production  ">
    <div id="wrapper">

      

      
      
      

      
      <div class="header header-logged-out">
  <div class="container clearfix">

      <a class="header-logo-wordmark" href="https://github.com/">Github</a>

    <div class="header-actions">
        <a class="button primary" href="https://github.com/signup">Sign up for free</a>
      <a class="button" href="https://github.com/login?return_to=%2Fipython%2Fipython%2Fblob%2Fmaster%2Fdocs%2Femacs%2Fipython.el">Sign in</a>
    </div>

      <ul class="top-nav">
          <li class="explore"><a href="https://github.com/explore">Explore GitHub</a></li>
        <li class="search"><a href="https://github.com/search">Search</a></li>
        <li class="features"><a href="https://github.com/features">Features</a></li>
          <li class="blog"><a href="https://github.com/blog">Blog</a></li>
      </ul>

  </div>
</div>


      

      


            <div class="site hfeed" itemscope itemtype="http://schema.org/WebPage">
      <div class="hentry">
        
        <div class="pagehead repohead instapaper_ignore readability-menu ">
          <div class="container">
            <div class="title-actions-bar">
              

<ul class="pagehead-actions">



    <li>
      <a href="/login?return_to=%2Fipython%2Fipython"
        class="minibutton js-toggler-target star-button entice tooltipped upwards"
        title="You must be signed in to use this feature" rel="nofollow">
        <span class="mini-icon mini-icon-star"></span>Star
      </a>
      <a class="social-count js-social-count" href="/ipython/ipython/stargazers">
        1,840
      </a>
    </li>
    <li>
      <a href="/login?return_to=%2Fipython%2Fipython"
        class="minibutton js-toggler-target fork-button entice tooltipped upwards"
        title="You must be signed in to fork a repository" rel="nofollow">
        <span class="mini-icon mini-icon-fork"></span>Fork
      </a>
      <a href="/ipython/ipython/network" class="social-count">
        565
      </a>
    </li>
</ul>

              <h1 itemscope itemtype="http://data-vocabulary.org/Breadcrumb" class="entry-title public">
                <span class="repo-label"><span>public</span></span>
                <span class="mega-icon mega-icon-public-repo"></span>
                <span class="author vcard">
                  <a href="/ipython" class="url fn" itemprop="url" rel="author">
                  <span itemprop="title">ipython</span>
                  </a></span> /
                <strong><a href="/ipython/ipython" class="js-current-repository">ipython</a></strong>
              </h1>
            </div>

            
  <ul class="tabs">
    <li class="pulse-nav"><a href="/ipython/ipython/pulse" highlight="pulse" rel="nofollow"><span class="mini-icon mini-icon-pulse"></span></a></li>
    <li><a href="/ipython/ipython" class="selected" highlight="repo_source repo_downloads repo_commits repo_tags repo_branches">Code</a></li>
    <li><a href="/ipython/ipython/network" highlight="repo_network">Network</a></li>
    <li><a href="/ipython/ipython/pulls" highlight="repo_pulls">Pull Requests <span class='counter'>12</span></a></li>

      <li><a href="/ipython/ipython/issues" highlight="repo_issues">Issues <span class='counter'>540</span></a></li>

      <li><a href="/ipython/ipython/wiki" highlight="repo_wiki">Wiki</a></li>


    <li><a href="/ipython/ipython/graphs" highlight="repo_graphs repo_contributors">Graphs</a></li>


  </ul>
  
<div class="tabnav">

  <span class="tabnav-right">
    <ul class="tabnav-tabs">
          <li><a href="/ipython/ipython/tags" class="tabnav-tab" highlight="repo_tags">Tags <span class="counter ">13</span></a></li>
    </ul>
    
  </span>

  <div class="tabnav-widget scope">


    <div class="select-menu js-menu-container js-select-menu js-branch-menu">
      <a class="minibutton select-menu-button js-menu-target" data-hotkey="w" data-ref="master">
        <span class="mini-icon mini-icon-branch"></span>
        <i>branch:</i>
        <span class="js-select-button">master</span>
      </a>

      <div class="select-menu-modal-holder js-menu-content js-navigation-container">

        <div class="select-menu-modal">
          <div class="select-menu-header">
            <span class="select-menu-title">Switch branches/tags</span>
            <span class="mini-icon mini-icon-remove-close js-menu-close"></span>
          </div> <!-- /.select-menu-header -->

          <div class="select-menu-filters">
            <div class="select-menu-text-filter">
              <input type="text" id="commitish-filter-field" class="js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
            </div>
            <div class="select-menu-tabs">
              <ul>
                <li class="select-menu-tab">
                  <a href="#" data-tab-filter="branches" class="js-select-menu-tab">Branches</a>
                </li>
                <li class="select-menu-tab">
                  <a href="#" data-tab-filter="tags" class="js-select-menu-tab">Tags</a>
                </li>
              </ul>
            </div><!-- /.select-menu-tabs -->
          </div><!-- /.select-menu-filters -->

          <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket css-truncate" data-tab-filter="branches">

            <div data-filterable-for="commitish-filter-field" data-filterable-type="substring">

                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/0.10.2/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="0.10.2" rel="nofollow" title="0.10.2">0.10.2</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/0.12.1/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="0.12.1" rel="nofollow" title="0.12.1">0.12.1</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/0.13.x/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="0.13.x" rel="nofollow" title="0.13.x">0.13.x</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item selected">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/master/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="master" rel="nofollow" title="master">master</a>
                </div> <!-- /.select-menu-item -->
            </div>

              <div class="select-menu-no-results">Nothing to show</div>
          </div> <!-- /.select-menu-list -->


          <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket css-truncate" data-tab-filter="tags">
            <div data-filterable-for="commitish-filter-field" data-filterable-type="substring">

                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.13.2/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.13.2" rel="nofollow" title="rel-0.13.2">rel-0.13.2</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.13.1/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.13.1" rel="nofollow" title="rel-0.13.1">rel-0.13.1</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.13/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.13" rel="nofollow" title="rel-0.13">rel-0.13</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.12.1/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.12.1" rel="nofollow" title="rel-0.12.1">rel-0.12.1</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.12/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.12" rel="nofollow" title="rel-0.12">rel-0.12</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.11/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.11" rel="nofollow" title="rel-0.11">rel-0.11</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.10.2/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.10.2" rel="nofollow" title="rel-0.10.2">rel-0.10.2</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.10.1/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.10.1" rel="nofollow" title="rel-0.10.1">rel-0.10.1</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.10/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.10" rel="nofollow" title="rel-0.10">rel-0.10</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.9.1/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.9.1" rel="nofollow" title="rel-0.9.1">rel-0.9.1</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.9/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.9" rel="nofollow" title="rel-0.9">rel-0.9</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/rel-0.8.4/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="rel-0.8.4" rel="nofollow" title="rel-0.8.4">rel-0.8.4</a>
                </div> <!-- /.select-menu-item -->
                <div class="select-menu-item js-navigation-item ">
                  <span class="select-menu-item-icon mini-icon mini-icon-confirm"></span>
                  <a href="/ipython/ipython/blob/dev-0.11/docs/emacs/ipython.el" class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target" data-name="dev-0.11" rel="nofollow" title="dev-0.11">dev-0.11</a>
                </div> <!-- /.select-menu-item -->
            </div>

            <div class="select-menu-no-results">Nothing to show</div>

          </div> <!-- /.select-menu-list -->

        </div> <!-- /.select-menu-modal -->
      </div> <!-- /.select-menu-modal-holder -->
    </div> <!-- /.select-menu -->

  </div> <!-- /.scope -->

  <ul class="tabnav-tabs">
    <li><a href="/ipython/ipython" class="selected tabnav-tab" highlight="repo_source">Files</a></li>
    <li><a href="/ipython/ipython/commits/master" class="tabnav-tab" highlight="repo_commits">Commits</a></li>
    <li><a href="/ipython/ipython/branches" class="tabnav-tab" highlight="repo_branches" rel="nofollow">Branches <span class="counter ">4</span></a></li>
  </ul>

</div>

  
  
  


            
          </div>
        </div><!-- /.repohead -->

        <div id="js-repo-pjax-container" class="container context-loader-container" data-pjax-container>
          


<!-- blob contrib key: blob_contributors:v21:2273bb670ba096ce4c19f98a1cc4e624 -->
<!-- blob contrib frag key: views10/v8/blob_contributors:v21:2273bb670ba096ce4c19f98a1cc4e624 -->


<div id="slider">
    <div class="frame-meta">

      <p title="This is a placeholder element" class="js-history-link-replace hidden"></p>

        <div class="breadcrumb">
          <span class='bold'><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/ipython/ipython" class="js-slide-to" data-branch="master" data-direction="back" itemscope="url"><span itemprop="title">ipython</span></a></span></span><span class="separator"> / </span><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/ipython/ipython/tree/master/docs" class="js-slide-to" data-branch="master" data-direction="back" itemscope="url"><span itemprop="title">docs</span></a></span><span class="separator"> / </span><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/ipython/ipython/tree/master/docs/emacs" class="js-slide-to" data-branch="master" data-direction="back" itemscope="url"><span itemprop="title">emacs</span></a></span><span class="separator"> / </span><strong class="final-path">ipython.el</strong> <span class="js-zeroclipboard zeroclipboard-button" data-clipboard-text="docs/emacs/ipython.el" data-copied-hint="copied!" title="copy to clipboard"><span class="mini-icon mini-icon-clipboard"></span></span>
        </div>

      <a href="/ipython/ipython/find/master" class="js-slide-to" data-hotkey="t" style="display:none">Show File Finder</a>


        
  <div class="commit file-history-tease">
    <img class="main-avatar" height="24" src="https://secure.gravatar.com/avatar/95198572b00e5fbcd97fb5315215bf7a?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
    <span class="author"><a href="/fperez" rel="author">fperez</a></span>
    <time class="js-relative-date" datetime="2011-11-24T15:56:46-08:00" title="2011-11-24 15:56:46">November 24, 2011</time>
    <div class="commit-title">
        <a href="/ipython/ipython/commit/4dbd4278d767e7a953059d00c2de9302d37e7275" class="message">Clarify docs about changing args to ipython in emacs support.</a>
    </div>

    <div class="participation">
      <p class="quickstat"><a href="#blob_contributors_box" rel="facebox"><strong>5</strong> contributors</a></p>
          <a class="avatar tooltipped downwards" title="paykroyd" href="/ipython/ipython/commits/master/docs/emacs/ipython.el?author=paykroyd"><img height="20" src="https://secure.gravatar.com/avatar/2c72ea94d9e24d79fe0f9242a46d7ac3?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /></a>
    <a class="avatar tooltipped downwards" title="tchaikov" href="/ipython/ipython/commits/master/docs/emacs/ipython.el?author=tchaikov"><img height="20" src="https://secure.gravatar.com/avatar/d41c179ff9ff34e26e5d582e983eae09?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /></a>
    <a class="avatar tooltipped downwards" title="takluyver" href="/ipython/ipython/commits/master/docs/emacs/ipython.el?author=takluyver"><img height="20" src="https://secure.gravatar.com/avatar/7f1ae3f14fd5b87c2c3f7b36014e185c?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /></a>
    <a class="avatar tooltipped downwards" title="ellisonbg" href="/ipython/ipython/commits/master/docs/emacs/ipython.el?author=ellisonbg"><img height="20" src="https://secure.gravatar.com/avatar/cae6da8d1ad0014fe4ac1d1e5acec7a3?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /></a>
    <a class="avatar tooltipped downwards" title="fperez" href="/ipython/ipython/commits/master/docs/emacs/ipython.el?author=fperez"><img height="20" src="https://secure.gravatar.com/avatar/95198572b00e5fbcd97fb5315215bf7a?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="20" /></a>


    </div>
    <div id="blob_contributors_box" style="display:none">
      <h2>Users on GitHub who have contributed to this file</h2>
      <ul class="facebox-user-list">
        <li>
          <img height="24" src="https://secure.gravatar.com/avatar/2c72ea94d9e24d79fe0f9242a46d7ac3?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/paykroyd">paykroyd</a>
        </li>
        <li>
          <img height="24" src="https://secure.gravatar.com/avatar/d41c179ff9ff34e26e5d582e983eae09?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/tchaikov">tchaikov</a>
        </li>
        <li>
          <img height="24" src="https://secure.gravatar.com/avatar/7f1ae3f14fd5b87c2c3f7b36014e185c?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/takluyver">takluyver</a>
        </li>
        <li>
          <img height="24" src="https://secure.gravatar.com/avatar/cae6da8d1ad0014fe4ac1d1e5acec7a3?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/ellisonbg">ellisonbg</a>
        </li>
        <li>
          <img height="24" src="https://secure.gravatar.com/avatar/95198572b00e5fbcd97fb5315215bf7a?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png" width="24" />
          <a href="/fperez">fperez</a>
        </li>
      </ul>
    </div>
  </div>


    </div><!-- ./.frame-meta -->

    <div class="frames">
      <div class="frame" data-permalink-url="/ipython/ipython/blob/4d76f67a84c0b184c855f234accf5de7d697afab/docs/emacs/ipython.el" data-title="ipython/docs/emacs/ipython.el at master · ipython/ipython · GitHub" data-type="blob">

        <div id="files" class="bubble">
          <div class="file">
            <div class="meta">
              <div class="info">
                <span class="icon"><b class="mini-icon mini-icon-text-file"></b></span>
                <span class="mode" title="File Mode">file</span>
                  <span>515 lines (469 sloc)</span>
                <span>21.8 kb</span>
              </div>
              <div class="actions">
                <div class="button-group">
                      <a class="minibutton js-entice" href=""
                         data-entice="You must be signed in and on a branch to make or propose changes">Edit</a>
                  <a href="/ipython/ipython/raw/master/docs/emacs/ipython.el" class="button minibutton " id="raw-url">Raw</a>
                    <a href="/ipython/ipython/blame/master/docs/emacs/ipython.el" class="button minibutton ">Blame</a>
                  <a href="/ipython/ipython/commits/master/docs/emacs/ipython.el" class="button minibutton " rel="nofollow">History</a>
                </div><!-- /.button-group -->
              </div><!-- /.actions -->

            </div>
                <div class="blob-wrapper data type-emacs-lisp js-blob-data">
      <table class="file-code file-diff">
        <tr class="file-code-line">
          <td class="blob-line-nums">
            <span id="L1" rel="#L1">1</span>
<span id="L2" rel="#L2">2</span>
<span id="L3" rel="#L3">3</span>
<span id="L4" rel="#L4">4</span>
<span id="L5" rel="#L5">5</span>
<span id="L6" rel="#L6">6</span>
<span id="L7" rel="#L7">7</span>
<span id="L8" rel="#L8">8</span>
<span id="L9" rel="#L9">9</span>
<span id="L10" rel="#L10">10</span>
<span id="L11" rel="#L11">11</span>
<span id="L12" rel="#L12">12</span>
<span id="L13" rel="#L13">13</span>
<span id="L14" rel="#L14">14</span>
<span id="L15" rel="#L15">15</span>
<span id="L16" rel="#L16">16</span>
<span id="L17" rel="#L17">17</span>
<span id="L18" rel="#L18">18</span>
<span id="L19" rel="#L19">19</span>
<span id="L20" rel="#L20">20</span>
<span id="L21" rel="#L21">21</span>
<span id="L22" rel="#L22">22</span>
<span id="L23" rel="#L23">23</span>
<span id="L24" rel="#L24">24</span>
<span id="L25" rel="#L25">25</span>
<span id="L26" rel="#L26">26</span>
<span id="L27" rel="#L27">27</span>
<span id="L28" rel="#L28">28</span>
<span id="L29" rel="#L29">29</span>
<span id="L30" rel="#L30">30</span>
<span id="L31" rel="#L31">31</span>
<span id="L32" rel="#L32">32</span>
<span id="L33" rel="#L33">33</span>
<span id="L34" rel="#L34">34</span>
<span id="L35" rel="#L35">35</span>
<span id="L36" rel="#L36">36</span>
<span id="L37" rel="#L37">37</span>
<span id="L38" rel="#L38">38</span>
<span id="L39" rel="#L39">39</span>
<span id="L40" rel="#L40">40</span>
<span id="L41" rel="#L41">41</span>
<span id="L42" rel="#L42">42</span>
<span id="L43" rel="#L43">43</span>
<span id="L44" rel="#L44">44</span>
<span id="L45" rel="#L45">45</span>
<span id="L46" rel="#L46">46</span>
<span id="L47" rel="#L47">47</span>
<span id="L48" rel="#L48">48</span>
<span id="L49" rel="#L49">49</span>
<span id="L50" rel="#L50">50</span>
<span id="L51" rel="#L51">51</span>
<span id="L52" rel="#L52">52</span>
<span id="L53" rel="#L53">53</span>
<span id="L54" rel="#L54">54</span>
<span id="L55" rel="#L55">55</span>
<span id="L56" rel="#L56">56</span>
<span id="L57" rel="#L57">57</span>
<span id="L58" rel="#L58">58</span>
<span id="L59" rel="#L59">59</span>
<span id="L60" rel="#L60">60</span>
<span id="L61" rel="#L61">61</span>
<span id="L62" rel="#L62">62</span>
<span id="L63" rel="#L63">63</span>
<span id="L64" rel="#L64">64</span>
<span id="L65" rel="#L65">65</span>
<span id="L66" rel="#L66">66</span>
<span id="L67" rel="#L67">67</span>
<span id="L68" rel="#L68">68</span>
<span id="L69" rel="#L69">69</span>
<span id="L70" rel="#L70">70</span>
<span id="L71" rel="#L71">71</span>
<span id="L72" rel="#L72">72</span>
<span id="L73" rel="#L73">73</span>
<span id="L74" rel="#L74">74</span>
<span id="L75" rel="#L75">75</span>
<span id="L76" rel="#L76">76</span>
<span id="L77" rel="#L77">77</span>
<span id="L78" rel="#L78">78</span>
<span id="L79" rel="#L79">79</span>
<span id="L80" rel="#L80">80</span>
<span id="L81" rel="#L81">81</span>
<span id="L82" rel="#L82">82</span>
<span id="L83" rel="#L83">83</span>
<span id="L84" rel="#L84">84</span>
<span id="L85" rel="#L85">85</span>
<span id="L86" rel="#L86">86</span>
<span id="L87" rel="#L87">87</span>
<span id="L88" rel="#L88">88</span>
<span id="L89" rel="#L89">89</span>
<span id="L90" rel="#L90">90</span>
<span id="L91" rel="#L91">91</span>
<span id="L92" rel="#L92">92</span>
<span id="L93" rel="#L93">93</span>
<span id="L94" rel="#L94">94</span>
<span id="L95" rel="#L95">95</span>
<span id="L96" rel="#L96">96</span>
<span id="L97" rel="#L97">97</span>
<span id="L98" rel="#L98">98</span>
<span id="L99" rel="#L99">99</span>
<span id="L100" rel="#L100">100</span>
<span id="L101" rel="#L101">101</span>
<span id="L102" rel="#L102">102</span>
<span id="L103" rel="#L103">103</span>
<span id="L104" rel="#L104">104</span>
<span id="L105" rel="#L105">105</span>
<span id="L106" rel="#L106">106</span>
<span id="L107" rel="#L107">107</span>
<span id="L108" rel="#L108">108</span>
<span id="L109" rel="#L109">109</span>
<span id="L110" rel="#L110">110</span>
<span id="L111" rel="#L111">111</span>
<span id="L112" rel="#L112">112</span>
<span id="L113" rel="#L113">113</span>
<span id="L114" rel="#L114">114</span>
<span id="L115" rel="#L115">115</span>
<span id="L116" rel="#L116">116</span>
<span id="L117" rel="#L117">117</span>
<span id="L118" rel="#L118">118</span>
<span id="L119" rel="#L119">119</span>
<span id="L120" rel="#L120">120</span>
<span id="L121" rel="#L121">121</span>
<span id="L122" rel="#L122">122</span>
<span id="L123" rel="#L123">123</span>
<span id="L124" rel="#L124">124</span>
<span id="L125" rel="#L125">125</span>
<span id="L126" rel="#L126">126</span>
<span id="L127" rel="#L127">127</span>
<span id="L128" rel="#L128">128</span>
<span id="L129" rel="#L129">129</span>
<span id="L130" rel="#L130">130</span>
<span id="L131" rel="#L131">131</span>
<span id="L132" rel="#L132">132</span>
<span id="L133" rel="#L133">133</span>
<span id="L134" rel="#L134">134</span>
<span id="L135" rel="#L135">135</span>
<span id="L136" rel="#L136">136</span>
<span id="L137" rel="#L137">137</span>
<span id="L138" rel="#L138">138</span>
<span id="L139" rel="#L139">139</span>
<span id="L140" rel="#L140">140</span>
<span id="L141" rel="#L141">141</span>
<span id="L142" rel="#L142">142</span>
<span id="L143" rel="#L143">143</span>
<span id="L144" rel="#L144">144</span>
<span id="L145" rel="#L145">145</span>
<span id="L146" rel="#L146">146</span>
<span id="L147" rel="#L147">147</span>
<span id="L148" rel="#L148">148</span>
<span id="L149" rel="#L149">149</span>
<span id="L150" rel="#L150">150</span>
<span id="L151" rel="#L151">151</span>
<span id="L152" rel="#L152">152</span>
<span id="L153" rel="#L153">153</span>
<span id="L154" rel="#L154">154</span>
<span id="L155" rel="#L155">155</span>
<span id="L156" rel="#L156">156</span>
<span id="L157" rel="#L157">157</span>
<span id="L158" rel="#L158">158</span>
<span id="L159" rel="#L159">159</span>
<span id="L160" rel="#L160">160</span>
<span id="L161" rel="#L161">161</span>
<span id="L162" rel="#L162">162</span>
<span id="L163" rel="#L163">163</span>
<span id="L164" rel="#L164">164</span>
<span id="L165" rel="#L165">165</span>
<span id="L166" rel="#L166">166</span>
<span id="L167" rel="#L167">167</span>
<span id="L168" rel="#L168">168</span>
<span id="L169" rel="#L169">169</span>
<span id="L170" rel="#L170">170</span>
<span id="L171" rel="#L171">171</span>
<span id="L172" rel="#L172">172</span>
<span id="L173" rel="#L173">173</span>
<span id="L174" rel="#L174">174</span>
<span id="L175" rel="#L175">175</span>
<span id="L176" rel="#L176">176</span>
<span id="L177" rel="#L177">177</span>
<span id="L178" rel="#L178">178</span>
<span id="L179" rel="#L179">179</span>
<span id="L180" rel="#L180">180</span>
<span id="L181" rel="#L181">181</span>
<span id="L182" rel="#L182">182</span>
<span id="L183" rel="#L183">183</span>
<span id="L184" rel="#L184">184</span>
<span id="L185" rel="#L185">185</span>
<span id="L186" rel="#L186">186</span>
<span id="L187" rel="#L187">187</span>
<span id="L188" rel="#L188">188</span>
<span id="L189" rel="#L189">189</span>
<span id="L190" rel="#L190">190</span>
<span id="L191" rel="#L191">191</span>
<span id="L192" rel="#L192">192</span>
<span id="L193" rel="#L193">193</span>
<span id="L194" rel="#L194">194</span>
<span id="L195" rel="#L195">195</span>
<span id="L196" rel="#L196">196</span>
<span id="L197" rel="#L197">197</span>
<span id="L198" rel="#L198">198</span>
<span id="L199" rel="#L199">199</span>
<span id="L200" rel="#L200">200</span>
<span id="L201" rel="#L201">201</span>
<span id="L202" rel="#L202">202</span>
<span id="L203" rel="#L203">203</span>
<span id="L204" rel="#L204">204</span>
<span id="L205" rel="#L205">205</span>
<span id="L206" rel="#L206">206</span>
<span id="L207" rel="#L207">207</span>
<span id="L208" rel="#L208">208</span>
<span id="L209" rel="#L209">209</span>
<span id="L210" rel="#L210">210</span>
<span id="L211" rel="#L211">211</span>
<span id="L212" rel="#L212">212</span>
<span id="L213" rel="#L213">213</span>
<span id="L214" rel="#L214">214</span>
<span id="L215" rel="#L215">215</span>
<span id="L216" rel="#L216">216</span>
<span id="L217" rel="#L217">217</span>
<span id="L218" rel="#L218">218</span>
<span id="L219" rel="#L219">219</span>
<span id="L220" rel="#L220">220</span>
<span id="L221" rel="#L221">221</span>
<span id="L222" rel="#L222">222</span>
<span id="L223" rel="#L223">223</span>
<span id="L224" rel="#L224">224</span>
<span id="L225" rel="#L225">225</span>
<span id="L226" rel="#L226">226</span>
<span id="L227" rel="#L227">227</span>
<span id="L228" rel="#L228">228</span>
<span id="L229" rel="#L229">229</span>
<span id="L230" rel="#L230">230</span>
<span id="L231" rel="#L231">231</span>
<span id="L232" rel="#L232">232</span>
<span id="L233" rel="#L233">233</span>
<span id="L234" rel="#L234">234</span>
<span id="L235" rel="#L235">235</span>
<span id="L236" rel="#L236">236</span>
<span id="L237" rel="#L237">237</span>
<span id="L238" rel="#L238">238</span>
<span id="L239" rel="#L239">239</span>
<span id="L240" rel="#L240">240</span>
<span id="L241" rel="#L241">241</span>
<span id="L242" rel="#L242">242</span>
<span id="L243" rel="#L243">243</span>
<span id="L244" rel="#L244">244</span>
<span id="L245" rel="#L245">245</span>
<span id="L246" rel="#L246">246</span>
<span id="L247" rel="#L247">247</span>
<span id="L248" rel="#L248">248</span>
<span id="L249" rel="#L249">249</span>
<span id="L250" rel="#L250">250</span>
<span id="L251" rel="#L251">251</span>
<span id="L252" rel="#L252">252</span>
<span id="L253" rel="#L253">253</span>
<span id="L254" rel="#L254">254</span>
<span id="L255" rel="#L255">255</span>
<span id="L256" rel="#L256">256</span>
<span id="L257" rel="#L257">257</span>
<span id="L258" rel="#L258">258</span>
<span id="L259" rel="#L259">259</span>
<span id="L260" rel="#L260">260</span>
<span id="L261" rel="#L261">261</span>
<span id="L262" rel="#L262">262</span>
<span id="L263" rel="#L263">263</span>
<span id="L264" rel="#L264">264</span>
<span id="L265" rel="#L265">265</span>
<span id="L266" rel="#L266">266</span>
<span id="L267" rel="#L267">267</span>
<span id="L268" rel="#L268">268</span>
<span id="L269" rel="#L269">269</span>
<span id="L270" rel="#L270">270</span>
<span id="L271" rel="#L271">271</span>
<span id="L272" rel="#L272">272</span>
<span id="L273" rel="#L273">273</span>
<span id="L274" rel="#L274">274</span>
<span id="L275" rel="#L275">275</span>
<span id="L276" rel="#L276">276</span>
<span id="L277" rel="#L277">277</span>
<span id="L278" rel="#L278">278</span>
<span id="L279" rel="#L279">279</span>
<span id="L280" rel="#L280">280</span>
<span id="L281" rel="#L281">281</span>
<span id="L282" rel="#L282">282</span>
<span id="L283" rel="#L283">283</span>
<span id="L284" rel="#L284">284</span>
<span id="L285" rel="#L285">285</span>
<span id="L286" rel="#L286">286</span>
<span id="L287" rel="#L287">287</span>
<span id="L288" rel="#L288">288</span>
<span id="L289" rel="#L289">289</span>
<span id="L290" rel="#L290">290</span>
<span id="L291" rel="#L291">291</span>
<span id="L292" rel="#L292">292</span>
<span id="L293" rel="#L293">293</span>
<span id="L294" rel="#L294">294</span>
<span id="L295" rel="#L295">295</span>
<span id="L296" rel="#L296">296</span>
<span id="L297" rel="#L297">297</span>
<span id="L298" rel="#L298">298</span>
<span id="L299" rel="#L299">299</span>
<span id="L300" rel="#L300">300</span>
<span id="L301" rel="#L301">301</span>
<span id="L302" rel="#L302">302</span>
<span id="L303" rel="#L303">303</span>
<span id="L304" rel="#L304">304</span>
<span id="L305" rel="#L305">305</span>
<span id="L306" rel="#L306">306</span>
<span id="L307" rel="#L307">307</span>
<span id="L308" rel="#L308">308</span>
<span id="L309" rel="#L309">309</span>
<span id="L310" rel="#L310">310</span>
<span id="L311" rel="#L311">311</span>
<span id="L312" rel="#L312">312</span>
<span id="L313" rel="#L313">313</span>
<span id="L314" rel="#L314">314</span>
<span id="L315" rel="#L315">315</span>
<span id="L316" rel="#L316">316</span>
<span id="L317" rel="#L317">317</span>
<span id="L318" rel="#L318">318</span>
<span id="L319" rel="#L319">319</span>
<span id="L320" rel="#L320">320</span>
<span id="L321" rel="#L321">321</span>
<span id="L322" rel="#L322">322</span>
<span id="L323" rel="#L323">323</span>
<span id="L324" rel="#L324">324</span>
<span id="L325" rel="#L325">325</span>
<span id="L326" rel="#L326">326</span>
<span id="L327" rel="#L327">327</span>
<span id="L328" rel="#L328">328</span>
<span id="L329" rel="#L329">329</span>
<span id="L330" rel="#L330">330</span>
<span id="L331" rel="#L331">331</span>
<span id="L332" rel="#L332">332</span>
<span id="L333" rel="#L333">333</span>
<span id="L334" rel="#L334">334</span>
<span id="L335" rel="#L335">335</span>
<span id="L336" rel="#L336">336</span>
<span id="L337" rel="#L337">337</span>
<span id="L338" rel="#L338">338</span>
<span id="L339" rel="#L339">339</span>
<span id="L340" rel="#L340">340</span>
<span id="L341" rel="#L341">341</span>
<span id="L342" rel="#L342">342</span>
<span id="L343" rel="#L343">343</span>
<span id="L344" rel="#L344">344</span>
<span id="L345" rel="#L345">345</span>
<span id="L346" rel="#L346">346</span>
<span id="L347" rel="#L347">347</span>
<span id="L348" rel="#L348">348</span>
<span id="L349" rel="#L349">349</span>
<span id="L350" rel="#L350">350</span>
<span id="L351" rel="#L351">351</span>
<span id="L352" rel="#L352">352</span>
<span id="L353" rel="#L353">353</span>
<span id="L354" rel="#L354">354</span>
<span id="L355" rel="#L355">355</span>
<span id="L356" rel="#L356">356</span>
<span id="L357" rel="#L357">357</span>
<span id="L358" rel="#L358">358</span>
<span id="L359" rel="#L359">359</span>
<span id="L360" rel="#L360">360</span>
<span id="L361" rel="#L361">361</span>
<span id="L362" rel="#L362">362</span>
<span id="L363" rel="#L363">363</span>
<span id="L364" rel="#L364">364</span>
<span id="L365" rel="#L365">365</span>
<span id="L366" rel="#L366">366</span>
<span id="L367" rel="#L367">367</span>
<span id="L368" rel="#L368">368</span>
<span id="L369" rel="#L369">369</span>
<span id="L370" rel="#L370">370</span>
<span id="L371" rel="#L371">371</span>
<span id="L372" rel="#L372">372</span>
<span id="L373" rel="#L373">373</span>
<span id="L374" rel="#L374">374</span>
<span id="L375" rel="#L375">375</span>
<span id="L376" rel="#L376">376</span>
<span id="L377" rel="#L377">377</span>
<span id="L378" rel="#L378">378</span>
<span id="L379" rel="#L379">379</span>
<span id="L380" rel="#L380">380</span>
<span id="L381" rel="#L381">381</span>
<span id="L382" rel="#L382">382</span>
<span id="L383" rel="#L383">383</span>
<span id="L384" rel="#L384">384</span>
<span id="L385" rel="#L385">385</span>
<span id="L386" rel="#L386">386</span>
<span id="L387" rel="#L387">387</span>
<span id="L388" rel="#L388">388</span>
<span id="L389" rel="#L389">389</span>
<span id="L390" rel="#L390">390</span>
<span id="L391" rel="#L391">391</span>
<span id="L392" rel="#L392">392</span>
<span id="L393" rel="#L393">393</span>
<span id="L394" rel="#L394">394</span>
<span id="L395" rel="#L395">395</span>
<span id="L396" rel="#L396">396</span>
<span id="L397" rel="#L397">397</span>
<span id="L398" rel="#L398">398</span>
<span id="L399" rel="#L399">399</span>
<span id="L400" rel="#L400">400</span>
<span id="L401" rel="#L401">401</span>
<span id="L402" rel="#L402">402</span>
<span id="L403" rel="#L403">403</span>
<span id="L404" rel="#L404">404</span>
<span id="L405" rel="#L405">405</span>
<span id="L406" rel="#L406">406</span>
<span id="L407" rel="#L407">407</span>
<span id="L408" rel="#L408">408</span>
<span id="L409" rel="#L409">409</span>
<span id="L410" rel="#L410">410</span>
<span id="L411" rel="#L411">411</span>
<span id="L412" rel="#L412">412</span>
<span id="L413" rel="#L413">413</span>
<span id="L414" rel="#L414">414</span>
<span id="L415" rel="#L415">415</span>
<span id="L416" rel="#L416">416</span>
<span id="L417" rel="#L417">417</span>
<span id="L418" rel="#L418">418</span>
<span id="L419" rel="#L419">419</span>
<span id="L420" rel="#L420">420</span>
<span id="L421" rel="#L421">421</span>
<span id="L422" rel="#L422">422</span>
<span id="L423" rel="#L423">423</span>
<span id="L424" rel="#L424">424</span>
<span id="L425" rel="#L425">425</span>
<span id="L426" rel="#L426">426</span>
<span id="L427" rel="#L427">427</span>
<span id="L428" rel="#L428">428</span>
<span id="L429" rel="#L429">429</span>
<span id="L430" rel="#L430">430</span>
<span id="L431" rel="#L431">431</span>
<span id="L432" rel="#L432">432</span>
<span id="L433" rel="#L433">433</span>
<span id="L434" rel="#L434">434</span>
<span id="L435" rel="#L435">435</span>
<span id="L436" rel="#L436">436</span>
<span id="L437" rel="#L437">437</span>
<span id="L438" rel="#L438">438</span>
<span id="L439" rel="#L439">439</span>
<span id="L440" rel="#L440">440</span>
<span id="L441" rel="#L441">441</span>
<span id="L442" rel="#L442">442</span>
<span id="L443" rel="#L443">443</span>
<span id="L444" rel="#L444">444</span>
<span id="L445" rel="#L445">445</span>
<span id="L446" rel="#L446">446</span>
<span id="L447" rel="#L447">447</span>
<span id="L448" rel="#L448">448</span>
<span id="L449" rel="#L449">449</span>
<span id="L450" rel="#L450">450</span>
<span id="L451" rel="#L451">451</span>
<span id="L452" rel="#L452">452</span>
<span id="L453" rel="#L453">453</span>
<span id="L454" rel="#L454">454</span>
<span id="L455" rel="#L455">455</span>
<span id="L456" rel="#L456">456</span>
<span id="L457" rel="#L457">457</span>
<span id="L458" rel="#L458">458</span>
<span id="L459" rel="#L459">459</span>
<span id="L460" rel="#L460">460</span>
<span id="L461" rel="#L461">461</span>
<span id="L462" rel="#L462">462</span>
<span id="L463" rel="#L463">463</span>
<span id="L464" rel="#L464">464</span>
<span id="L465" rel="#L465">465</span>
<span id="L466" rel="#L466">466</span>
<span id="L467" rel="#L467">467</span>
<span id="L468" rel="#L468">468</span>
<span id="L469" rel="#L469">469</span>
<span id="L470" rel="#L470">470</span>
<span id="L471" rel="#L471">471</span>
<span id="L472" rel="#L472">472</span>
<span id="L473" rel="#L473">473</span>
<span id="L474" rel="#L474">474</span>
<span id="L475" rel="#L475">475</span>
<span id="L476" rel="#L476">476</span>
<span id="L477" rel="#L477">477</span>
<span id="L478" rel="#L478">478</span>
<span id="L479" rel="#L479">479</span>
<span id="L480" rel="#L480">480</span>
<span id="L481" rel="#L481">481</span>
<span id="L482" rel="#L482">482</span>
<span id="L483" rel="#L483">483</span>
<span id="L484" rel="#L484">484</span>
<span id="L485" rel="#L485">485</span>
<span id="L486" rel="#L486">486</span>
<span id="L487" rel="#L487">487</span>
<span id="L488" rel="#L488">488</span>
<span id="L489" rel="#L489">489</span>
<span id="L490" rel="#L490">490</span>
<span id="L491" rel="#L491">491</span>
<span id="L492" rel="#L492">492</span>
<span id="L493" rel="#L493">493</span>
<span id="L494" rel="#L494">494</span>
<span id="L495" rel="#L495">495</span>
<span id="L496" rel="#L496">496</span>
<span id="L497" rel="#L497">497</span>
<span id="L498" rel="#L498">498</span>
<span id="L499" rel="#L499">499</span>
<span id="L500" rel="#L500">500</span>
<span id="L501" rel="#L501">501</span>
<span id="L502" rel="#L502">502</span>
<span id="L503" rel="#L503">503</span>
<span id="L504" rel="#L504">504</span>
<span id="L505" rel="#L505">505</span>
<span id="L506" rel="#L506">506</span>
<span id="L507" rel="#L507">507</span>
<span id="L508" rel="#L508">508</span>
<span id="L509" rel="#L509">509</span>
<span id="L510" rel="#L510">510</span>
<span id="L511" rel="#L511">511</span>
<span id="L512" rel="#L512">512</span>
<span id="L513" rel="#L513">513</span>
<span id="L514" rel="#L514">514</span>

          </td>
          <td class="blob-line-code">
                  <div class="highlight"><pre><div class='line' id='LC1'><span class="c1">;;; ipython.el --- Adds support for IPython to python-mode.el</span></div><div class='line' id='LC2'><br/></div><div class='line' id='LC3'><span class="c1">;; Copyright (C) 2002, 2003, 2004, 2005 Alexander Schmolck</span></div><div class='line' id='LC4'><span class="c1">;; Author:        Alexander Schmolck</span></div><div class='line' id='LC5'><span class="c1">;; Keywords:      ipython python languages oop</span></div><div class='line' id='LC6'><span class="c1">;; URL:           http://ipython.org</span></div><div class='line' id='LC7'><span class="c1">;; Compatibility: Emacs21, XEmacs21</span></div><div class='line' id='LC8'><span class="c1">;; FIXME: #$@! INPUT RING</span></div><div class='line' id='LC9'><span class="p">(</span><span class="nf">defconst</span> <span class="nv">ipython-version</span> <span class="s">&quot;0.11&quot;</span></div><div class='line' id='LC10'>&nbsp;&nbsp;<span class="s">&quot;Tied to IPython main version number.&quot;</span><span class="p">)</span></div><div class='line' id='LC11'><br/></div><div class='line' id='LC12'><span class="c1">;;; Commentary</span></div><div class='line' id='LC13'><span class="c1">;; This library makes all the functionality python-mode has when running with</span></div><div class='line' id='LC14'><span class="c1">;; the normal python-interpreter available for ipython, too. It also enables a</span></div><div class='line' id='LC15'><span class="c1">;; persistent py-shell command history across sessions (if you exit python</span></div><div class='line' id='LC16'><span class="c1">;; with C-d in py-shell) and defines the command `ipython-to-doctest&#39;, which</span></div><div class='line' id='LC17'><span class="c1">;; can be used to convert bits of a ipython session into something that can be</span></div><div class='line' id='LC18'><span class="c1">;; used for doctests. To install, put this file somewhere in your emacs</span></div><div class='line' id='LC19'><span class="c1">;; `load-path&#39; [1] and add the following line to your ~/.emacs file (the first</span></div><div class='line' id='LC20'><span class="c1">;; line only needed if the default (``&quot;ipython&quot;``) is wrong or ipython is not </span></div><div class='line' id='LC21'><span class="c1">;; in your `exec-path&#39;)::</span></div><div class='line' id='LC22'><span class="c1">;;</span></div><div class='line' id='LC23'><span class="c1">;;   (setq ipython-command &quot;/SOME-PATH/ipython&quot;)</span></div><div class='line' id='LC24'><span class="c1">;;   (require &#39;ipython)</span></div><div class='line' id='LC25'><span class="c1">;;</span></div><div class='line' id='LC26'><span class="c1">;; Ipython will be set as the default python shell, but only if the ipython</span></div><div class='line' id='LC27'><span class="c1">;; executable is in the path. For ipython sessions autocompletion with &lt;tab&gt;</span></div><div class='line' id='LC28'><span class="c1">;; is also enabled (experimental feature!). Please also note that all the</span></div><div class='line' id='LC29'><span class="c1">;; terminal functions in py-shell are handled by emacs&#39;s comint, **not** by</span></div><div class='line' id='LC30'><span class="c1">;; (i)python, so importing readline etc. will have 0 effect.</span></div><div class='line' id='LC31'><span class="c1">;;</span></div><div class='line' id='LC32'><span class="c1">;; To start an interactive ipython session run `py-shell&#39; with ``M-x py-shell``</span></div><div class='line' id='LC33'><span class="c1">;; (or the default keybinding ``C-c C-!``).</span></div><div class='line' id='LC34'><span class="c1">;;</span></div><div class='line' id='LC35'><span class="c1">;; You can customize the arguments passed to the IPython instance at startup by</span></div><div class='line' id='LC36'><span class="c1">;; setting the ``py-python-command-args`` variable.  For example, to start</span></div><div class='line' id='LC37'><span class="c1">;; always in ``pylab`` mode with hardcoded light-background colors, you can</span></div><div class='line' id='LC38'><span class="c1">;; use the following, *after* the ``(require &#39;ipython)`` line::</span></div><div class='line' id='LC39'><span class="c1">;;</span></div><div class='line' id='LC40'><span class="c1">;; (setq-default py-python-command-args &#39;(&quot;--pylab&quot; &quot;--colors=LightBG&quot;))</span></div><div class='line' id='LC41'><span class="c1">;;</span></div><div class='line' id='LC42'><span class="c1">;;</span></div><div class='line' id='LC43'><span class="c1">;; NOTE: This mode is currently somewhat alpha and although I hope that it</span></div><div class='line' id='LC44'><span class="c1">;; will work fine for most cases, doing certain things (like the</span></div><div class='line' id='LC45'><span class="c1">;; autocompletion and a decent scheme to switch between python interpreters)</span></div><div class='line' id='LC46'><span class="c1">;; properly will also require changes to ipython that will likely have to wait</span></div><div class='line' id='LC47'><span class="c1">;; for a larger rewrite scheduled some time in the future.</span></div><div class='line' id='LC48'><span class="c1">;;</span></div><div class='line' id='LC49'><span class="c1">;;</span></div><div class='line' id='LC50'><span class="c1">;; Further note that I don&#39;t know whether this runs under windows or not and</span></div><div class='line' id='LC51'><span class="c1">;; that if it doesn&#39;t I can&#39;t really help much, not being afflicted myself.</span></div><div class='line' id='LC52'><span class="c1">;;</span></div><div class='line' id='LC53'><span class="c1">;;</span></div><div class='line' id='LC54'><span class="c1">;; Hints for effective usage</span></div><div class='line' id='LC55'><span class="c1">;; -------------------------</span></div><div class='line' id='LC56'><span class="c1">;;</span></div><div class='line' id='LC57'><span class="c1">;; - IMO the best feature by far of the ipython/emacs combo is how much easier</span></div><div class='line' id='LC58'><span class="c1">;;   it makes it to find and fix bugs thanks to the ``%pdb on or %debug``/</span></div><div class='line' id='LC59'><span class="c1">;;   pdbtrack combo. **NOTE** for this feature to work, you must turn coloring</span></div><div class='line' id='LC60'><span class="c1">;;   off, at least during your debug session.  Type ``%colors nocolor`` before</span></div><div class='line' id='LC61'><span class="c1">;;   debugging and file tracking will work, you can re-enable it with ``%colors</span></div><div class='line' id='LC62'><span class="c1">;;   linux`` or ``%colors lightbg`` (depending on your preference) when</span></div><div class='line' id='LC63'><span class="c1">;;   finished debugging so you can have coloring for the rest of the session.</span></div><div class='line' id='LC64'><span class="c1">;;</span></div><div class='line' id='LC65'><span class="c1">;;   Try it: first in the ipython to shell do ``%pdb on`` then do something</span></div><div class='line' id='LC66'><span class="c1">;;   that will raise an exception (FIXME nice example), or type ``%debug``</span></div><div class='line' id='LC67'><span class="c1">;;   after the exception has been raised.  You&#39;ll be amazed at how easy it is</span></div><div class='line' id='LC68'><span class="c1">;;   to inspect the live objects in each stack frames and to jump to the</span></div><div class='line' id='LC69'><span class="c1">;;   corresponding sourcecode locations as you walk up and down the stack trace</span></div><div class='line' id='LC70'><span class="c1">;;   (even without ``%pdb on`` you can always use ``C-c -`` (`py-up-exception&#39;)</span></div><div class='line' id='LC71'><span class="c1">;;   to jump to the corresponding source code locations).</span></div><div class='line' id='LC72'><span class="c1">;;</span></div><div class='line' id='LC73'><span class="c1">;; - emacs gives you much more powerful commandline editing and output searching</span></div><div class='line' id='LC74'><span class="c1">;;   capabilities than ipython-standalone -- isearch is your friend if you</span></div><div class='line' id='LC75'><span class="c1">;;   quickly want to print &#39;DEBUG ...&#39; to stdout out etc.</span></div><div class='line' id='LC76'><span class="c1">;;</span></div><div class='line' id='LC77'><span class="c1">;; - This is not really specific to ipython, but for more convenient history</span></div><div class='line' id='LC78'><span class="c1">;;   access you might want to add something like the following to *the beggining*</span></div><div class='line' id='LC79'><span class="c1">;;   of your ``.emacs`` (if you want behavior that&#39;s more similar to stand-alone</span></div><div class='line' id='LC80'><span class="c1">;;   ipython, you can change ``meta p`` etc. for ``control p``)::</span></div><div class='line' id='LC81'><span class="c1">;;</span></div><div class='line' id='LC82'><span class="c1">;;         (require &#39;comint)</span></div><div class='line' id='LC83'><span class="c1">;;         (define-key comint-mode-map [(meta p)]</span></div><div class='line' id='LC84'><span class="c1">;;           &#39;comint-previous-matching-input-from-input)</span></div><div class='line' id='LC85'><span class="c1">;;         (define-key comint-mode-map [(meta n)]</span></div><div class='line' id='LC86'><span class="c1">;;           &#39;comint-next-matching-input-from-input)</span></div><div class='line' id='LC87'><span class="c1">;;         (define-key comint-mode-map [(control meta n)]</span></div><div class='line' id='LC88'><span class="c1">;;            &#39;comint-next-input)</span></div><div class='line' id='LC89'><span class="c1">;;         (define-key comint-mode-map [(control meta p)]</span></div><div class='line' id='LC90'><span class="c1">;;            &#39;comint-previous-input)</span></div><div class='line' id='LC91'><span class="c1">;;</span></div><div class='line' id='LC92'><span class="c1">;; - Be aware that if you customize py-python-command previously, this value</span></div><div class='line' id='LC93'><span class="c1">;;   will override what ipython.el does (because loading the customization</span></div><div class='line' id='LC94'><span class="c1">;;   variables comes later).</span></div><div class='line' id='LC95'><span class="c1">;;</span></div><div class='line' id='LC96'><span class="c1">;; Please send comments and feedback to the ipython-list</span></div><div class='line' id='LC97'><span class="c1">;; (&lt;ipython-user@scipy.org&gt;) where I (a.s.) or someone else will try to</span></div><div class='line' id='LC98'><span class="c1">;; answer them (it helps if you specify your emacs version, OS etc;</span></div><div class='line' id='LC99'><span class="c1">;; familiarity with &lt;http://www.catb.org/~esr/faqs/smart-questions.html&gt; might</span></div><div class='line' id='LC100'><span class="c1">;; speed up things further).</span></div><div class='line' id='LC101'><span class="c1">;;</span></div><div class='line' id='LC102'><span class="c1">;; Footnotes:</span></div><div class='line' id='LC103'><span class="c1">;;</span></div><div class='line' id='LC104'><span class="c1">;;     [1] If you don&#39;t know what `load-path&#39; is, C-h v load-path will tell</span></div><div class='line' id='LC105'><span class="c1">;;     you; if required you can also add a new directory. So assuming that</span></div><div class='line' id='LC106'><span class="c1">;;     ipython.el resides in ~/el/, put this in your emacs:</span></div><div class='line' id='LC107'><span class="c1">;;</span></div><div class='line' id='LC108'><span class="c1">;;</span></div><div class='line' id='LC109'><span class="c1">;;           (add-to-list &#39;load-path &quot;~/el&quot;)</span></div><div class='line' id='LC110'><span class="c1">;;           (setq ipython-command &quot;/some-path/ipython&quot;)</span></div><div class='line' id='LC111'><span class="c1">;;           (require &#39;ipython)</span></div><div class='line' id='LC112'><span class="c1">;;</span></div><div class='line' id='LC113'><span class="c1">;;</span></div><div class='line' id='LC114'><span class="c1">;;</span></div><div class='line' id='LC115'><span class="c1">;;</span></div><div class='line' id='LC116'><span class="c1">;; TODO:</span></div><div class='line' id='LC117'><span class="c1">;;      - do autocompletion properly</span></div><div class='line' id='LC118'><span class="c1">;;      - implement a proper switching between python interpreters</span></div><div class='line' id='LC119'><span class="c1">;;</span></div><div class='line' id='LC120'><span class="c1">;; BUGS:</span></div><div class='line' id='LC121'><span class="c1">;;      - neither::</span></div><div class='line' id='LC122'><span class="c1">;;</span></div><div class='line' id='LC123'><span class="c1">;;         (py-shell &quot;-c print &#39;FOOBAR&#39;&quot;)</span></div><div class='line' id='LC124'><span class="c1">;;</span></div><div class='line' id='LC125'><span class="c1">;;        nor::</span></div><div class='line' id='LC126'><span class="c1">;;</span></div><div class='line' id='LC127'><span class="c1">;;         (let ((py-python-command-args (append py-python-command-args</span></div><div class='line' id='LC128'><span class="c1">;;                                              &#39;(&quot;-c&quot; &quot;print &#39;FOOBAR&#39;&quot;))))</span></div><div class='line' id='LC129'><span class="c1">;;           (py-shell))</span></div><div class='line' id='LC130'><span class="c1">;;</span></div><div class='line' id='LC131'><span class="c1">;;        seem to print anything as they should</span></div><div class='line' id='LC132'><span class="c1">;;</span></div><div class='line' id='LC133'><span class="c1">;;      - look into init priority issues with `py-python-command&#39; (if it&#39;s set</span></div><div class='line' id='LC134'><span class="c1">;;        via custom)</span></div><div class='line' id='LC135'><br/></div><div class='line' id='LC136'><br/></div><div class='line' id='LC137'><span class="c1">;;; Code</span></div><div class='line' id='LC138'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;cl</span><span class="p">)</span></div><div class='line' id='LC139'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;shell</span><span class="p">)</span></div><div class='line' id='LC140'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;executable</span><span class="p">)</span></div><div class='line' id='LC141'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;ansi-color</span><span class="p">)</span></div><div class='line' id='LC142'><span class="c1">;; XXX load python-mode, so that we can screw around with its variables</span></div><div class='line' id='LC143'><span class="c1">;; this has the disadvantage that python-mode is loaded even if no</span></div><div class='line' id='LC144'><span class="c1">;; python-file is ever edited etc. but it means that `py-shell&#39; works</span></div><div class='line' id='LC145'><span class="c1">;; without loading a python-file first. Obviously screwing around with</span></div><div class='line' id='LC146'><span class="c1">;; python-mode&#39;s variables like this is a mess, but well.</span></div><div class='line' id='LC147'><span class="p">(</span><span class="nf">require</span> <span class="ss">&#39;python-mode</span><span class="p">)</span></div><div class='line' id='LC148'><br/></div><div class='line' id='LC149'><span class="p">(</span><span class="nf">defcustom</span> <span class="nv">ipython-command</span> <span class="s">&quot;ipython&quot;</span></div><div class='line' id='LC150'>&nbsp;&nbsp;<span class="s">&quot;*Shell command used to start ipython.&quot;</span></div><div class='line' id='LC151'>&nbsp;&nbsp;<span class="nv">:type</span> <span class="ss">&#39;string</span></div><div class='line' id='LC152'>&nbsp;&nbsp;<span class="nv">:group</span> <span class="ss">&#39;python</span><span class="p">)</span></div><div class='line' id='LC153'><br/></div><div class='line' id='LC154'><span class="c1">;; Users can set this to nil</span></div><div class='line' id='LC155'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">py-shell-initial-switch-buffers</span> <span class="nv">t</span></div><div class='line' id='LC156'>&nbsp;&nbsp;<span class="s">&quot;If nil, don&#39;t switch to the *Python* buffer on the first call to</span></div><div class='line' id='LC157'><span class="s">  `py-shell&#39;.&quot;</span><span class="p">)</span></div><div class='line' id='LC158'><br/></div><div class='line' id='LC159'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">ipython-backup-of-py-python-command</span> <span class="nv">nil</span></div><div class='line' id='LC160'>&nbsp;&nbsp;<span class="s">&quot;HACK&quot;</span><span class="p">)</span></div><div class='line' id='LC161'><br/></div><div class='line' id='LC162'><br/></div><div class='line' id='LC163'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">ipython-de-input-prompt-regexp</span> <span class="s">&quot;\\(?:</span></div><div class='line' id='LC164'><span class="s">In \\[[0-9]+\\]: *.*</span></div><div class='line' id='LC165'><span class="s">----+&gt; \\(.*</span></div><div class='line' id='LC166'><span class="s">\\)[\n]?\\)\\|\\(?:</span></div><div class='line' id='LC167'><span class="s">In \\[[0-9]+\\]: *\\(.*</span></div><div class='line' id='LC168'><span class="s">\\)\\)\\|^[ ]\\{3\\}[.]\\{3,\\}: *\\(.*</span></div><div class='line' id='LC169'><span class="s">\\)&quot;</span></div><div class='line' id='LC170'>&nbsp;&nbsp;<span class="s">&quot;A regular expression to match the IPython input prompt and the python</span></div><div class='line' id='LC171'><span class="s">command after it. The first match group is for a command that is rewritten,</span></div><div class='line' id='LC172'><span class="s">the second for a &#39;normal&#39; command, and the third for a multiline command.&quot;</span><span class="p">)</span></div><div class='line' id='LC173'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">ipython-de-output-prompt-regexp</span> <span class="s">&quot;^Out\\[[0-9]+\\]: &quot;</span></div><div class='line' id='LC174'>&nbsp;&nbsp;<span class="s">&quot;A regular expression to match the output prompt of IPython.&quot;</span><span class="p">)</span></div><div class='line' id='LC175'><br/></div><div class='line' id='LC176'><br/></div><div class='line' id='LC177'><span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nb">not </span><span class="p">(</span><span class="nf">executable-find</span> <span class="nv">ipython-command</span><span class="p">))</span></div><div class='line' id='LC178'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">message</span> <span class="p">(</span><span class="nf">format</span> <span class="s">&quot;Can&#39;t find executable %s - ipython.el *NOT* activated!!!&quot;</span></div><div class='line' id='LC179'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">ipython-command</span><span class="p">))</span></div><div class='line' id='LC180'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; change the default value of py-shell-name to ipython</span></div><div class='line' id='LC181'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq-default</span> <span class="nv">py-shell-name</span> <span class="nv">ipython-command</span><span class="p">)</span></div><div class='line' id='LC182'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; turn on ansi colors for ipython and activate completion</span></div><div class='line' id='LC183'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">defun</span> <span class="nv">ipython-shell-hook</span> <span class="p">()</span></div><div class='line' id='LC184'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; the following is to synchronize dir-changes</span></div><div class='line' id='LC185'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">make-local-variable</span> <span class="ss">&#39;shell-dirstack</span><span class="p">)</span></div><div class='line' id='LC186'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">shell-dirstack</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC187'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">make-local-variable</span> <span class="ss">&#39;shell-last-dir</span><span class="p">)</span></div><div class='line' id='LC188'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">shell-last-dir</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC189'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">make-local-variable</span> <span class="ss">&#39;shell-dirtrackp</span><span class="p">)</span></div><div class='line' id='LC190'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">shell-dirtrackp</span> <span class="nv">t</span><span class="p">)</span></div><div class='line' id='LC191'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">add-hook</span> <span class="ss">&#39;comint-input-filter-functions</span> <span class="ss">&#39;shell-directory-tracker</span> <span class="nv">nil</span> <span class="nv">t</span><span class="p">)</span></div><div class='line' id='LC192'><br/></div><div class='line' id='LC193'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">ansi-color-for-comint-mode-on</span><span class="p">)</span></div><div class='line' id='LC194'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">define-key</span> <span class="nv">py-shell-map</span> <span class="p">[</span><span class="nv">tab</span><span class="p">]</span> <span class="ss">&#39;ipython-complete</span><span class="p">)</span></div><div class='line' id='LC195'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; Add this so that tab-completion works both in X11 frames and inside</span></div><div class='line' id='LC196'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; terminals (such as when emacs is called with -nw).</span></div><div class='line' id='LC197'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">define-key</span> <span class="nv">py-shell-map</span> <span class="s">&quot;\t&quot;</span> <span class="ss">&#39;ipython-complete</span><span class="p">)</span></div><div class='line' id='LC198'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;XXX this is really just a cheap hack, it only completes symbols in the</span></div><div class='line' id='LC199'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;interactive session -- useful nonetheless.</span></div><div class='line' id='LC200'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">define-key</span> <span class="nv">py-mode-map</span> <span class="p">[(</span><span class="nf">meta</span> <span class="nv">tab</span><span class="p">)]</span> <span class="ss">&#39;ipython-complete</span><span class="p">)</span></div><div class='line' id='LC201'><br/></div><div class='line' id='LC202'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">)</span></div><div class='line' id='LC203'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">add-hook</span> <span class="ss">&#39;py-shell-hook</span> <span class="ss">&#39;ipython-shell-hook</span><span class="p">)</span></div><div class='line' id='LC204'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; Regular expression that describes tracebacks for IPython in context and</span></div><div class='line' id='LC205'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; verbose mode.</span></div><div class='line' id='LC206'><br/></div><div class='line' id='LC207'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;Adapt python-mode settings for ipython.</span></div><div class='line' id='LC208'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; (this works for %xmode &#39;verbose&#39; or &#39;context&#39;)</span></div><div class='line' id='LC209'><br/></div><div class='line' id='LC210'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; XXX putative regexps for syntax errors; unfortunately the</span></div><div class='line' id='LC211'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;     current python-mode traceback-line-re scheme is too primitive,</span></div><div class='line' id='LC212'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;     so it&#39;s either matching syntax errors, *or* everything else</span></div><div class='line' id='LC213'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;     (XXX: should ask Fernando for a change)</span></div><div class='line' id='LC214'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;&quot;^   File \&quot;\\(.*?\\)\&quot;, line \\([0-9]+\\).*\n.*\n.*\nSyntaxError:&quot;</span></div><div class='line' id='LC215'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;^   File \&quot;\\(.*?\\)\&quot;, line \\([0-9]+\\)&quot;</span></div><div class='line' id='LC216'><br/></div><div class='line' id='LC217'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">py-traceback-line-re</span></div><div class='line' id='LC218'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;\\(^[^\t &gt;].+?\\.py\\).*\n   +[0-9]+[^\00]*?\n-+&gt; \\([0-9]+\\)+&quot;</span><span class="p">)</span></div><div class='line' id='LC219'><br/></div><div class='line' id='LC220'><br/></div><div class='line' id='LC221'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; Recognize the ipython pdb, whose prompt is &#39;ipdb&gt;&#39; or  &#39;ipydb&gt;&#39;</span></div><div class='line' id='LC222'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;instead of &#39;(Pdb)&#39;</span></div><div class='line' id='LC223'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">py-pdbtrack-input-prompt</span> <span class="s">&quot;\n[(&lt;]*[Ii]?[Pp]y?db[&gt;)]+ &quot;</span><span class="p">)</span></div><div class='line' id='LC224'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">pydb-pydbtrack-input-prompt</span> <span class="s">&quot;\n[(]*ipydb[&gt;)]+ &quot;</span><span class="p">)</span></div><div class='line' id='LC225'><br/></div><div class='line' id='LC226'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">py-shell-input-prompt-1-regexp</span> <span class="s">&quot;^In \\[[0-9]+\\]: *&quot;</span></div><div class='line' id='LC227'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">py-shell-input-prompt-2-regexp</span> <span class="s">&quot;^   [.][.][.]+: *&quot;</span> <span class="p">)</span></div><div class='line' id='LC228'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; select a suitable color-scheme</span></div><div class='line' id='LC229'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">unless</span> <span class="p">(</span><span class="nf">delq</span> <span class="nv">nil</span></div><div class='line' id='LC230'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">mapcar</span> <span class="p">(</span><span class="k">lambda </span><span class="p">(</span><span class="nf">x</span><span class="p">)</span> <span class="p">(</span><span class="nf">eq</span> <span class="p">(</span><span class="nf">string-match</span> <span class="s">&quot;^--colors*&quot;</span> <span class="nv">x</span><span class="p">)</span> <span class="mi">0</span><span class="p">))</span></div><div class='line' id='LC231'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">py-python-command-args</span><span class="p">))</span></div><div class='line' id='LC232'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq-default</span> <span class="nv">py-python-command-args</span></div><div class='line' id='LC233'>		    <span class="p">(</span><span class="nb">cons </span><span class="p">(</span><span class="nf">format</span> <span class="s">&quot;--colors=%s&quot;</span></div><div class='line' id='LC234'>				  <span class="p">(</span><span class="nf">cond</span></div><div class='line' id='LC235'>				   <span class="p">((</span><span class="nf">eq</span> <span class="nv">frame-background-mode</span> <span class="ss">&#39;dark</span><span class="p">)</span></div><div class='line' id='LC236'>				    <span class="s">&quot;Linux&quot;</span><span class="p">)</span></div><div class='line' id='LC237'>				   <span class="p">((</span><span class="nf">eq</span> <span class="nv">frame-background-mode</span> <span class="ss">&#39;light</span><span class="p">)</span></div><div class='line' id='LC238'>				    <span class="s">&quot;LightBG&quot;</span><span class="p">)</span></div><div class='line' id='LC239'>				   <span class="p">(</span><span class="nf">t</span> <span class="c1">; default (backg-mode isn&#39;t always set by XEmacs)</span></div><div class='line' id='LC240'>				    <span class="s">&quot;LightBG&quot;</span><span class="p">)))</span></div><div class='line' id='LC241'>			  <span class="nv">py-python-command-args</span><span class="p">)))</span></div><div class='line' id='LC242'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">when</span> <span class="p">(</span><span class="nf">boundp</span> <span class="ss">&#39;py-python-command</span><span class="p">)</span></div><div class='line' id='LC243'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">unless</span> <span class="p">(</span><span class="nf">equal</span> <span class="nv">ipython-backup-of-py-python-command</span> <span class="nv">py-python-command</span><span class="p">)</span></div><div class='line' id='LC244'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">ipython-backup-of-py-python-command</span> <span class="nv">py-python-command</span><span class="p">)))</span></div><div class='line' id='LC245'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">py-python-command</span> <span class="nv">ipython-command</span><span class="p">)</span></div><div class='line' id='LC246'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">when</span> <span class="p">(</span><span class="nf">boundp</span> <span class="ss">&#39;py-shell-name</span><span class="p">)</span></div><div class='line' id='LC247'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">py-shell-name</span> <span class="nv">ipython-command</span><span class="p">)))</span></div><div class='line' id='LC248'><br/></div><div class='line' id='LC249'><span class="c1">;; MODIFY py-shell so that it loads the editing history</span></div><div class='line' id='LC250'><span class="p">(</span><span class="nf">defadvice</span> <span class="nv">py-shell</span> <span class="p">(</span><span class="nf">around</span> <span class="nv">py-shell-with-history</span><span class="p">)</span></div><div class='line' id='LC251'>&nbsp;&nbsp;<span class="s">&quot;Add persistent command-history support (in</span></div><div class='line' id='LC252'><span class="s">$PYTHONHISTORY (or \&quot;~/.ipython/history\&quot;, if we use IPython)). Also, if</span></div><div class='line' id='LC253'><span class="s">`py-shell-initial-switch-buffers&#39; is nil, it only switches to *Python* if that</span></div><div class='line' id='LC254'><span class="s">buffer already exists.&quot;</span></div><div class='line' id='LC255'>&nbsp;&nbsp;<span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nf">comint-check-proc</span> <span class="s">&quot;*Python*&quot;</span><span class="p">)</span></div><div class='line' id='LC256'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">ad-do-it</span></div><div class='line' id='LC257'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">comint-input-ring-file-name</span></div><div class='line' id='LC258'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nf">string-equal</span> <span class="nv">py-python-command</span> <span class="nv">ipython-command</span><span class="p">)</span></div><div class='line' id='LC259'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">concat</span> <span class="p">(</span><span class="k">or </span><span class="p">(</span><span class="nf">getenv</span> <span class="s">&quot;IPYTHONDIR&quot;</span><span class="p">)</span> <span class="s">&quot;~/.ipython&quot;</span><span class="p">)</span> <span class="s">&quot;/history&quot;</span><span class="p">)</span></div><div class='line' id='LC260'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">or </span><span class="p">(</span><span class="nf">getenv</span> <span class="s">&quot;PYTHONHISTORY&quot;</span><span class="p">)</span> <span class="s">&quot;~/.python-history.py&quot;</span><span class="p">)))</span></div><div class='line' id='LC261'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">comint-read-input-ring</span> <span class="nv">t</span><span class="p">)</span></div><div class='line' id='LC262'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">buf</span> <span class="p">(</span><span class="nf">current-buffer</span><span class="p">)))</span></div><div class='line' id='LC263'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">ad-do-it</span></div><div class='line' id='LC264'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">unless</span> <span class="nv">py-shell-initial-switch-buffers</span></div><div class='line' id='LC265'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">switch-to-buffer-other-window</span> <span class="nv">buf</span><span class="p">)))))</span></div><div class='line' id='LC266'><span class="p">(</span><span class="nf">ad-activate</span> <span class="ss">&#39;py-shell</span><span class="p">)</span></div><div class='line' id='LC267'><span class="c1">;; (defadvice py-execute-region (before py-execute-buffer-ensure-process)</span></div><div class='line' id='LC268'><span class="c1">;;   &quot;HACK: test that ipython is already running before executing something.</span></div><div class='line' id='LC269'><span class="c1">;;   Doing this properly seems not worth the bother (unless people actually</span></div><div class='line' id='LC270'><span class="c1">;;   request it).&quot;</span></div><div class='line' id='LC271'><span class="c1">;; (unless (comint-check-proc &quot;*Python*&quot;)</span></div><div class='line' id='LC272'><span class="c1">;;     (error &quot;Sorry you have to first do M-x py-shell to send something to ipython.&quot;)))</span></div><div class='line' id='LC273'><span class="c1">;; (ad-activate &#39;py-execute-region)</span></div><div class='line' id='LC274'><br/></div><div class='line' id='LC275'><span class="p">(</span><span class="nf">defadvice</span> <span class="nv">py-execute-region</span> <span class="p">(</span><span class="nf">around</span> <span class="nv">py-execute-buffer-ensure-process</span><span class="p">)</span></div><div class='line' id='LC276'>&nbsp;&nbsp;<span class="s">&quot;HACK: if `py-shell&#39; is not active or ASYNC is explicitly desired, fall back</span></div><div class='line' id='LC277'><span class="s">  to python instead of ipython.&quot;</span></div><div class='line' id='LC278'>&nbsp;&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">py-which-shell</span> <span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="k">and </span><span class="p">(</span><span class="nf">comint-check-proc</span> <span class="s">&quot;*Python*&quot;</span><span class="p">)</span> <span class="p">(</span><span class="nb">not </span><span class="nv">async</span><span class="p">))</span></div><div class='line' id='LC279'>			    <span class="nv">py-python-command</span></div><div class='line' id='LC280'>			  <span class="nv">ipython-backup-of-py-python-command</span><span class="p">)))</span></div><div class='line' id='LC281'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">ad-do-it</span><span class="p">))</span></div><div class='line' id='LC282'><span class="p">(</span><span class="nf">ad-activate</span> <span class="ss">&#39;py-execute-region</span><span class="p">)</span></div><div class='line' id='LC283'><br/></div><div class='line' id='LC284'><span class="p">(</span><span class="nf">defun</span> <span class="nv">ipython-to-doctest</span> <span class="p">(</span><span class="nf">start</span> <span class="nv">end</span><span class="p">)</span></div><div class='line' id='LC285'>&nbsp;&nbsp;<span class="s">&quot;Transform a cut-and-pasted bit from an IPython session into something that</span></div><div class='line' id='LC286'><span class="s">looks like it came from a normal interactive python session, so that it can</span></div><div class='line' id='LC287'><span class="s">be used in doctests. Example:</span></div><div class='line' id='LC288'><br/></div><div class='line' id='LC289'><br/></div><div class='line' id='LC290'><span class="s">    In [1]: import sys</span></div><div class='line' id='LC291'><br/></div><div class='line' id='LC292'><span class="s">    In [2]: sys.stdout.write &#39;Hi!\n&#39;</span></div><div class='line' id='LC293'><span class="s">    ------&gt; sys.stdout.write (&#39;Hi!\n&#39;)</span></div><div class='line' id='LC294'><span class="s">    Hi!</span></div><div class='line' id='LC295'><br/></div><div class='line' id='LC296'><span class="s">    In [3]: 3 + 4</span></div><div class='line' id='LC297'><span class="s">    Out[3]: 7</span></div><div class='line' id='LC298'><br/></div><div class='line' id='LC299'><span class="s">gets converted to:</span></div><div class='line' id='LC300'><br/></div><div class='line' id='LC301'><span class="s">    &gt;&gt;&gt; import sys</span></div><div class='line' id='LC302'><span class="s">    &gt;&gt;&gt; sys.stdout.write (&#39;Hi!\n&#39;)</span></div><div class='line' id='LC303'><span class="s">    Hi!</span></div><div class='line' id='LC304'><span class="s">    &gt;&gt;&gt; 3 + 4</span></div><div class='line' id='LC305'><span class="s">    7</span></div><div class='line' id='LC306'><br/></div><div class='line' id='LC307'><span class="s">&quot;</span></div><div class='line' id='LC308'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span> <span class="s">&quot;*r\n&quot;</span><span class="p">)</span></div><div class='line' id='LC309'>&nbsp;&nbsp;<span class="c1">;(message (format &quot;###DEBUG s:%de:%d&quot; start end))</span></div><div class='line' id='LC310'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">save-excursion</span></div><div class='line' id='LC311'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">save-match-data</span></div><div class='line' id='LC312'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; replace ``In [3]: bla`` with ``&gt;&gt;&gt; bla`` and</span></div><div class='line' id='LC313'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;         ``... :   bla`` with ``...    bla``</span></div><div class='line' id='LC314'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">goto-char</span> <span class="nv">start</span><span class="p">)</span></div><div class='line' id='LC315'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">while</span> <span class="p">(</span><span class="nf">re-search-forward</span> <span class="nv">ipython-de-input-prompt-regexp</span> <span class="nv">end</span> <span class="nv">t</span><span class="p">)</span></div><div class='line' id='LC316'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;(message &quot;finding 1&quot;)</span></div><div class='line' id='LC317'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">cond </span><span class="p">((</span><span class="nf">match-string</span> <span class="mi">3</span><span class="p">)</span>         <span class="c1">;continued</span></div><div class='line' id='LC318'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">replace-match</span> <span class="s">&quot;... \\3&quot;</span> <span class="nv">t</span> <span class="nv">nil</span><span class="p">))</span></div><div class='line' id='LC319'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">t</span></div><div class='line' id='LC320'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">replace-match</span> <span class="s">&quot;&gt;&gt;&gt; \\1\\2&quot;</span> <span class="nv">t</span> <span class="nv">nil</span><span class="p">))))</span></div><div class='line' id='LC321'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; replace ``</span></div><div class='line' id='LC322'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">goto-char</span> <span class="nv">start</span><span class="p">)</span></div><div class='line' id='LC323'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">while</span> <span class="p">(</span><span class="nf">re-search-forward</span> <span class="nv">ipython-de-output-prompt-regexp</span> <span class="nv">end</span> <span class="nv">t</span><span class="p">)</span></div><div class='line' id='LC324'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">replace-match</span> <span class="s">&quot;&quot;</span> <span class="nv">t</span> <span class="nv">nil</span><span class="p">)))))</span></div><div class='line' id='LC325'><br/></div><div class='line' id='LC326'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">ipython-completion-command-string</span></div><div class='line' id='LC327'>&nbsp;&nbsp;<span class="s">&quot;print(&#39;;&#39;.join(get_ipython().complete(&#39;%s&#39;, &#39;%s&#39;)[1])) #PYTHON-MODE SILENT\n&quot;</span></div><div class='line' id='LC328'>&nbsp;&nbsp;<span class="s">&quot;The string send to ipython to query for all possible completions&quot;</span><span class="p">)</span></div><div class='line' id='LC329'><br/></div><div class='line' id='LC330'><br/></div><div class='line' id='LC331'><span class="c1">;; xemacs doesn&#39;t have `comint-preoutput-filter-functions&#39; so we&#39;ll try the</span></div><div class='line' id='LC332'><span class="c1">;; following wonderful hack to work around this case</span></div><div class='line' id='LC333'><span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nf">featurep</span> <span class="ss">&#39;xemacs</span><span class="p">)</span></div><div class='line' id='LC334'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;;xemacs</span></div><div class='line' id='LC335'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">defun</span> <span class="nv">ipython-complete</span> <span class="p">()</span></div><div class='line' id='LC336'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;Try to complete the python symbol before point. Only knows about the stuff</span></div><div class='line' id='LC337'><span class="s">in the current *Python* session.&quot;</span></div><div class='line' id='LC338'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC339'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let* </span><span class="p">((</span><span class="nf">ugly-return</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC340'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">sep</span> <span class="s">&quot;;&quot;</span><span class="p">)</span></div><div class='line' id='LC341'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">python-process</span> <span class="p">(</span><span class="k">or </span><span class="p">(</span><span class="nf">get-buffer-process</span> <span class="p">(</span><span class="nf">current-buffer</span><span class="p">))</span></div><div class='line' id='LC342'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;XXX hack for .py buffers</span></div><div class='line' id='LC343'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">get-process</span> <span class="nv">py-which-bufname</span><span class="p">)))</span></div><div class='line' id='LC344'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; XXX currently we go backwards to find the beginning of an</span></div><div class='line' id='LC345'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; expression part; a more powerful approach in the future might be</span></div><div class='line' id='LC346'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; to let ipython have the complete line, so that context can be used</span></div><div class='line' id='LC347'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; to do things like filename completion etc.</span></div><div class='line' id='LC348'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">beg</span> <span class="p">(</span><span class="nf">save-excursion</span> <span class="p">(</span><span class="nf">skip-chars-backward</span> <span class="s">&quot;a-z0-9A-Z_.&quot;</span> <span class="p">(</span><span class="nf">point-at-bol</span><span class="p">))</span></div><div class='line' id='LC349'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">point</span><span class="p">)))</span></div><div class='line' id='LC350'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">end</span> <span class="p">(</span><span class="nf">point</span><span class="p">))</span></div><div class='line' id='LC351'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">pattern</span> <span class="p">(</span><span class="nf">buffer-substring-no-properties</span> <span class="nv">beg</span> <span class="nv">end</span><span class="p">))</span></div><div class='line' id='LC352'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">completions</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC353'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">completion-table</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC354'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">completion</span></div><div class='line' id='LC355'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">comint-output-filter-functions</span></div><div class='line' id='LC356'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">append </span><span class="nv">comint-output-filter-functions</span></div><div class='line' id='LC357'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">&#39;</span><span class="p">(</span><span class="nv">ansi-color-filter-apply</span></div><div class='line' id='LC358'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">lambda </span><span class="p">(</span><span class="nf">string</span><span class="p">)</span></div><div class='line' id='LC359'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;(message (format &quot;DEBUG filtering: %s&quot; string))</span></div><div class='line' id='LC360'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">ugly-return</span> <span class="p">(</span><span class="nf">concat</span> <span class="nv">ugly-return</span> <span class="nv">string</span><span class="p">))</span></div><div class='line' id='LC361'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">delete-region</span> <span class="nv">comint-last-output-start</span></div><div class='line' id='LC362'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">process-mark</span> <span class="p">(</span><span class="nf">get-buffer-process</span> <span class="p">(</span><span class="nf">current-buffer</span><span class="p">)))))))))</span></div><div class='line' id='LC363'>	<span class="c1">;(message (format &quot;#DEBUG pattern: &#39;%s&#39;&quot; pattern))</span></div><div class='line' id='LC364'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">process-send-string</span> <span class="nv">python-process</span></div><div class='line' id='LC365'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">format</span> <span class="nv">ipython-completion-command-string</span> <span class="nv">pattern</span><span class="p">))</span></div><div class='line' id='LC366'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">accept-process-output</span> <span class="nv">python-process</span><span class="p">)</span></div><div class='line' id='LC367'><br/></div><div class='line' id='LC368'>	<span class="c1">;(message (format &quot;DEBUG return: %s&quot; ugly-return))</span></div><div class='line' id='LC369'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">completions</span></div><div class='line' id='LC370'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">split-string</span> <span class="p">(</span><span class="nb">substring </span><span class="nv">ugly-return</span> <span class="mi">0</span> <span class="p">(</span><span class="nf">position</span> <span class="nv">?</span><span class="err">\</span><span class="nv">n</span> <span class="nv">ugly-return</span><span class="p">))</span> <span class="nv">sep</span><span class="p">))</span></div><div class='line' id='LC371'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">completion-table</span> <span class="p">(</span><span class="nf">loop</span> <span class="nv">for</span> <span class="nv">str</span> <span class="nv">in</span> <span class="nv">completions</span></div><div class='line' id='LC372'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">collect</span> <span class="p">(</span><span class="nb">list </span><span class="nv">str</span> <span class="nv">nil</span><span class="p">)))</span></div><div class='line' id='LC373'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">completion</span> <span class="p">(</span><span class="nf">try-completion</span> <span class="nv">pattern</span> <span class="nv">completion-table</span><span class="p">))</span></div><div class='line' id='LC374'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">cond </span><span class="p">((</span><span class="nf">eq</span> <span class="nv">completion</span> <span class="nv">t</span><span class="p">))</span></div><div class='line' id='LC375'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nf">null</span> <span class="nv">completion</span><span class="p">)</span></div><div class='line' id='LC376'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">message</span> <span class="s">&quot;Can&#39;t find completion for \&quot;%s\&quot;&quot;</span> <span class="nv">pattern</span><span class="p">)</span></div><div class='line' id='LC377'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">ding</span><span class="p">))</span></div><div class='line' id='LC378'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">not </span><span class="p">(</span><span class="nf">string=</span> <span class="nv">pattern</span> <span class="nv">completion</span><span class="p">))</span></div><div class='line' id='LC379'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">delete-region</span> <span class="nv">beg</span> <span class="nv">end</span><span class="p">)</span></div><div class='line' id='LC380'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="nv">completion</span><span class="p">))</span></div><div class='line' id='LC381'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">t</span></div><div class='line' id='LC382'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">message</span> <span class="s">&quot;Making completion list...&quot;</span><span class="p">)</span></div><div class='line' id='LC383'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">with-output-to-temp-buffer</span> <span class="s">&quot;*Python Completions*&quot;</span></div><div class='line' id='LC384'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">display-completion-list</span> <span class="p">(</span><span class="nf">all-completions</span> <span class="nv">pattern</span> <span class="nv">completion-table</span><span class="p">)))</span></div><div class='line' id='LC385'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">message</span> <span class="s">&quot;Making completion list...%s&quot;</span> <span class="s">&quot;done&quot;</span><span class="p">)))))</span></div><div class='line' id='LC386'>&nbsp;&nbsp;<span class="c1">;; emacs</span></div><div class='line' id='LC387'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">defun</span> <span class="nv">ipython-complete</span> <span class="p">()</span></div><div class='line' id='LC388'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;Try to complete the python symbol before point. Only knows about the stuff</span></div><div class='line' id='LC389'><span class="s">in the current *Python* session.&quot;</span></div><div class='line' id='LC390'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC391'>&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let* </span><span class="p">((</span><span class="nf">ugly-return</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC392'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">sep</span> <span class="s">&quot;;&quot;</span><span class="p">)</span></div><div class='line' id='LC393'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">python-process</span> <span class="p">(</span><span class="k">or </span><span class="p">(</span><span class="nf">get-buffer-process</span> <span class="p">(</span><span class="nf">current-buffer</span><span class="p">))</span></div><div class='line' id='LC394'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;XXX hack for .py buffers</span></div><div class='line' id='LC395'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">get-process</span> <span class="nv">py-which-bufname</span><span class="p">)))</span></div><div class='line' id='LC396'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; XXX currently we go backwards to find the beginning of an</span></div><div class='line' id='LC397'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; expression part; a more powerful approach in the future might be</span></div><div class='line' id='LC398'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; to let ipython have the complete line, so that context can be used</span></div><div class='line' id='LC399'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; to do things like filename completion etc.</span></div><div class='line' id='LC400'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">beg</span> <span class="p">(</span><span class="nf">save-excursion</span> <span class="p">(</span><span class="nf">skip-chars-backward</span> <span class="s">&quot;a-z0-9A-Z_./\-&quot;</span> <span class="p">(</span><span class="nf">point-at-bol</span><span class="p">))</span></div><div class='line' id='LC401'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">point</span><span class="p">)))</span></div><div class='line' id='LC402'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">end</span> <span class="p">(</span><span class="nf">point</span><span class="p">))</span></div><div class='line' id='LC403'>	   <span class="p">(</span><span class="nf">line</span> <span class="p">(</span><span class="nf">buffer-substring-no-properties</span> <span class="p">(</span><span class="nf">point-at-bol</span><span class="p">)</span> <span class="nv">end</span><span class="p">))</span></div><div class='line' id='LC404'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">pattern</span> <span class="p">(</span><span class="nf">buffer-substring-no-properties</span> <span class="nv">beg</span> <span class="nv">end</span><span class="p">))</span></div><div class='line' id='LC405'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">completions</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC406'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">completion-table</span> <span class="nv">nil</span><span class="p">)</span></div><div class='line' id='LC407'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">completion</span></div><div class='line' id='LC408'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">comint-preoutput-filter-functions</span></div><div class='line' id='LC409'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">append </span><span class="nv">comint-preoutput-filter-functions</span></div><div class='line' id='LC410'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">&#39;</span><span class="p">(</span><span class="nv">ansi-color-filter-apply</span></div><div class='line' id='LC411'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">lambda </span><span class="p">(</span><span class="nf">string</span><span class="p">)</span></div><div class='line' id='LC412'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">ugly-return</span> <span class="p">(</span><span class="nf">concat</span> <span class="nv">ugly-return</span> <span class="nv">string</span><span class="p">))</span></div><div class='line' id='LC413'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s">&quot;&quot;</span><span class="p">)))))</span></div><div class='line' id='LC414'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">process-send-string</span> <span class="nv">python-process</span></div><div class='line' id='LC415'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">format</span> <span class="nv">ipython-completion-command-string</span> <span class="nv">pattern</span> <span class="nv">line</span><span class="p">))</span></div><div class='line' id='LC416'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">accept-process-output</span> <span class="nv">python-process</span><span class="p">)</span></div><div class='line' id='LC417'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">completions</span></div><div class='line' id='LC418'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">split-string</span> <span class="p">(</span><span class="nb">substring </span><span class="nv">ugly-return</span> <span class="mi">0</span> <span class="p">(</span><span class="nf">position</span> <span class="nv">?</span><span class="err">\</span><span class="nv">n</span> <span class="nv">ugly-return</span><span class="p">))</span> <span class="nv">sep</span><span class="p">))</span></div><div class='line' id='LC419'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;(message (format &quot;DEBUG completions: %S&quot; completions))</span></div><div class='line' id='LC420'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">completion-table</span> <span class="p">(</span><span class="nf">loop</span> <span class="nv">for</span> <span class="nv">str</span> <span class="nv">in</span> <span class="nv">completions</span></div><div class='line' id='LC421'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">collect</span> <span class="p">(</span><span class="nb">list </span><span class="nv">str</span> <span class="nv">nil</span><span class="p">)))</span></div><div class='line' id='LC422'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">completion</span> <span class="p">(</span><span class="nf">try-completion</span> <span class="nv">pattern</span> <span class="nv">completion-table</span><span class="p">))</span></div><div class='line' id='LC423'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">cond </span><span class="p">((</span><span class="nf">eq</span> <span class="nv">completion</span> <span class="nv">t</span><span class="p">))</span></div><div class='line' id='LC424'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nf">null</span> <span class="nv">completion</span><span class="p">)</span></div><div class='line' id='LC425'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">message</span> <span class="s">&quot;Can&#39;t find completion for \&quot;%s\&quot; based on line %s&quot;</span> <span class="nv">pattern</span> <span class="nv">line</span><span class="p">)</span></div><div class='line' id='LC426'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">ding</span><span class="p">))</span></div><div class='line' id='LC427'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">not </span><span class="p">(</span><span class="nf">string=</span> <span class="nv">pattern</span> <span class="nv">completion</span><span class="p">))</span></div><div class='line' id='LC428'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">delete-region</span> <span class="p">(</span><span class="nb">- </span><span class="nv">end</span> <span class="p">(</span><span class="nb">length </span><span class="nv">pattern</span><span class="p">))</span> <span class="nv">end</span><span class="p">)</span></div><div class='line' id='LC429'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="nv">completion</span><span class="p">))</span></div><div class='line' id='LC430'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">t</span></div><div class='line' id='LC431'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">message</span> <span class="s">&quot;Making completion list...&quot;</span><span class="p">)</span></div><div class='line' id='LC432'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">with-output-to-temp-buffer</span> <span class="s">&quot;*IPython Completions*&quot;</span></div><div class='line' id='LC433'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">display-completion-list</span> <span class="p">(</span><span class="nf">all-completions</span> <span class="nv">pattern</span> <span class="nv">completion-table</span><span class="p">)))</span></div><div class='line' id='LC434'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">message</span> <span class="s">&quot;Making completion list...%s&quot;</span> <span class="s">&quot;done&quot;</span><span class="p">)))))</span></div><div class='line' id='LC435'><span class="p">)</span></div><div class='line' id='LC436'><br/></div><div class='line' id='LC437'><span class="c1">;;; if python-mode&#39;s keybinding for the tab key wins then py-shell-complete is called</span></div><div class='line' id='LC438'><span class="c1">;;; instead of ipython-complete which result in hanging emacs since there is no shell</span></div><div class='line' id='LC439'><span class="c1">;;; process for python-mode to communicate with</span></div><div class='line' id='LC440'><span class="p">(</span><span class="nf">defadvice</span> <span class="nv">py-shell-complete</span></div><div class='line' id='LC441'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">around</span> <span class="nv">avoid-py-shell-complete</span> <span class="nv">activate</span><span class="p">)</span></div><div class='line' id='LC442'>&nbsp;&nbsp;<span class="p">(</span><span class="nf">ipython-complete</span><span class="p">))</span></div><div class='line' id='LC443'><br/></div><div class='line' id='LC444'><br/></div><div class='line' id='LC445'><span class="c1">;;; autoindent support: patch sent in by Jin Liu &lt;m.liu.jin@gmail.com&gt;,</span></div><div class='line' id='LC446'><span class="c1">;;; originally written by doxgen@newsmth.net</span></div><div class='line' id='LC447'><span class="c1">;;; Minor modifications by fperez for xemacs compatibility.</span></div><div class='line' id='LC448'><br/></div><div class='line' id='LC449'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">ipython-autoindent</span> <span class="nv">t</span></div><div class='line' id='LC450'>&nbsp;<span class="s">&quot;If non-nil, enable autoindent for IPython shell through python-mode.&quot;</span><span class="p">)</span></div><div class='line' id='LC451'><br/></div><div class='line' id='LC452'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">ipython-indenting-buffer-name</span> <span class="s">&quot;*IPython Indentation Calculation*&quot;</span></div><div class='line' id='LC453'>&nbsp;<span class="s">&quot;Temporary buffer for indenting multiline statement.&quot;</span><span class="p">)</span></div><div class='line' id='LC454'><br/></div><div class='line' id='LC455'><span class="p">(</span><span class="nf">defun</span> <span class="nv">ipython-get-indenting-buffer</span> <span class="p">()</span></div><div class='line' id='LC456'>&nbsp;<span class="s">&quot;Return a temporary buffer set in python-mode. Create one if necessary.&quot;</span></div><div class='line' id='LC457'>&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">buf</span> <span class="p">(</span><span class="nf">get-buffer-create</span> <span class="nv">ipython-indenting-buffer-name</span><span class="p">)))</span></div><div class='line' id='LC458'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">set-buffer</span> <span class="nv">buf</span><span class="p">)</span></div><div class='line' id='LC459'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">unless</span> <span class="p">(</span><span class="nf">eq</span> <span class="nv">major-mode</span> <span class="ss">&#39;python-mode</span><span class="p">)</span></div><div class='line' id='LC460'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">python-mode</span><span class="p">))</span></div><div class='line' id='LC461'>&nbsp;&nbsp;&nbsp;<span class="nv">buf</span><span class="p">))</span></div><div class='line' id='LC462'><br/></div><div class='line' id='LC463'><span class="p">(</span><span class="nf">defvar</span> <span class="nv">ipython-indentation-string</span> <span class="nv">nil</span></div><div class='line' id='LC464'>&nbsp;<span class="s">&quot;Indentation for the next line in a multiline statement.&quot;</span><span class="p">)</span></div><div class='line' id='LC465'><br/></div><div class='line' id='LC466'><span class="p">(</span><span class="nf">defun</span> <span class="nv">ipython-send-and-indent</span> <span class="p">()</span></div><div class='line' id='LC467'>&nbsp;<span class="s">&quot;Send the current line to IPython, and calculate the indentation for</span></div><div class='line' id='LC468'><span class="s">the next line.&quot;</span></div><div class='line' id='LC469'>&nbsp;<span class="p">(</span><span class="nf">interactive</span><span class="p">)</span></div><div class='line' id='LC470'>&nbsp;<span class="p">(</span><span class="k">if </span><span class="nv">ipython-autoindent</span></div><div class='line' id='LC471'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let </span><span class="p">((</span><span class="nf">line</span> <span class="p">(</span><span class="nf">buffer-substring</span> <span class="p">(</span><span class="nf">point-at-bol</span><span class="p">)</span> <span class="p">(</span><span class="nf">point</span><span class="p">)))</span></div><div class='line' id='LC472'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">after-prompt1</span><span class="p">)</span></div><div class='line' id='LC473'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">after-prompt2</span><span class="p">))</span></div><div class='line' id='LC474'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">save-excursion</span></div><div class='line' id='LC475'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">comint-bol</span> <span class="nv">t</span><span class="p">)</span></div><div class='line' id='LC476'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if </span><span class="p">(</span><span class="nf">looking-at</span> <span class="nv">py-shell-input-prompt-1-regexp</span><span class="p">)</span></div><div class='line' id='LC477'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">after-prompt1</span> <span class="nv">t</span><span class="p">)</span></div><div class='line' id='LC478'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">after-prompt2</span> <span class="p">(</span><span class="nf">looking-at</span> <span class="nv">py-shell-input-prompt-2-regexp</span><span class="p">)))</span></div><div class='line' id='LC479'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">with-current-buffer</span> <span class="p">(</span><span class="nf">ipython-get-indenting-buffer</span><span class="p">)</span></div><div class='line' id='LC480'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">when</span> <span class="nv">after-prompt1</span></div><div class='line' id='LC481'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">erase-buffer</span><span class="p">))</span></div><div class='line' id='LC482'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">when</span> <span class="p">(</span><span class="k">or </span><span class="nv">after-prompt1</span> <span class="nv">after-prompt2</span><span class="p">)</span></div><div class='line' id='LC483'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">delete-region</span> <span class="p">(</span><span class="nf">point-at-bol</span><span class="p">)</span> <span class="p">(</span><span class="nf">point</span><span class="p">))</span></div><div class='line' id='LC484'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="nv">line</span><span class="p">)</span></div><div class='line' id='LC485'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">newline-and-indent</span><span class="p">))))))</span></div><div class='line' id='LC486'>&nbsp;<span class="c1">;; send input line to ipython interpreter</span></div><div class='line' id='LC487'>&nbsp;<span class="p">(</span><span class="nf">comint-send-input</span><span class="p">))</span></div><div class='line' id='LC488'><br/></div><div class='line' id='LC489'><span class="p">(</span><span class="nf">defun</span> <span class="nv">ipython-indentation-hook</span> <span class="p">(</span><span class="nf">string</span><span class="p">)</span></div><div class='line' id='LC490'>&nbsp;<span class="s">&quot;Insert indentation string if py-shell-input-prompt-2-regexp</span></div><div class='line' id='LC491'><span class="s">matches last process output.&quot;</span></div><div class='line' id='LC492'>&nbsp;<span class="p">(</span><span class="k">let* </span><span class="p">((</span><span class="nf">start-marker</span> <span class="p">(</span><span class="k">or </span><span class="nv">comint-last-output-start</span></div><div class='line' id='LC493'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">point-min-marker</span><span class="p">)))</span></div><div class='line' id='LC494'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">end-marker</span> <span class="p">(</span><span class="nf">process-mark</span> <span class="p">(</span><span class="nf">get-buffer-process</span> <span class="p">(</span><span class="nf">current-buffer</span><span class="p">))))</span></div><div class='line' id='LC495'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">text</span> <span class="p">(</span><span class="nf">ansi-color-filter-apply</span> <span class="p">(</span><span class="nf">buffer-substring</span> <span class="nv">start-marker</span> <span class="nv">end-marker</span><span class="p">))))</span></div><div class='line' id='LC496'>&nbsp;&nbsp;&nbsp;<span class="c1">;; XXX if `text&#39; matches both pattern, it MUST be the last prompt-2</span></div><div class='line' id='LC497'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">when</span> <span class="p">(</span><span class="k">and </span><span class="p">(</span><span class="nf">string-match</span> <span class="nv">py-shell-input-prompt-2-regexp</span> <span class="nv">text</span><span class="p">)</span></div><div class='line' id='LC498'>	      <span class="p">(</span><span class="nb">not </span><span class="p">(</span><span class="nf">string-match</span> <span class="s">&quot;\n$&quot;</span> <span class="nv">text</span><span class="p">)))</span></div><div class='line' id='LC499'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">with-current-buffer</span> <span class="p">(</span><span class="nf">ipython-get-indenting-buffer</span><span class="p">)</span></div><div class='line' id='LC500'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">ipython-indentation-string</span></div><div class='line' id='LC501'>	     <span class="p">(</span><span class="nf">buffer-substring</span> <span class="p">(</span><span class="nf">point-at-bol</span><span class="p">)</span> <span class="p">(</span><span class="nf">point</span><span class="p">))))</span></div><div class='line' id='LC502'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">goto-char</span> <span class="nv">end-marker</span><span class="p">)</span></div><div class='line' id='LC503'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">insert</span> <span class="nv">ipython-indentation-string</span><span class="p">)</span></div><div class='line' id='LC504'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">setq</span> <span class="nv">ipython-indentation-string</span> <span class="nv">nil</span><span class="p">))))</span></div><div class='line' id='LC505'><br/></div><div class='line' id='LC506'><span class="p">(</span><span class="nf">add-hook</span> <span class="ss">&#39;py-shell-hook</span></div><div class='line' id='LC507'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">lambda </span><span class="p">()</span></div><div class='line' id='LC508'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nf">add-hook</span> <span class="ss">&#39;comint-output-filter-functions</span></div><div class='line' id='LC509'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">&#39;ipython-indentation-hook</span><span class="p">)))</span></div><div class='line' id='LC510'><br/></div><div class='line' id='LC511'><span class="p">(</span><span class="nf">define-key</span> <span class="nv">py-shell-map</span> <span class="p">(</span><span class="nf">kbd</span> <span class="s">&quot;RET&quot;</span><span class="p">)</span> <span class="ss">&#39;ipython-send-and-indent</span><span class="p">)</span></div><div class='line' id='LC512'><span class="c1">;;; / end autoindent support</span></div><div class='line' id='LC513'><br/></div><div class='line' id='LC514'><span class="p">(</span><span class="nf">provide</span> <span class="ss">&#39;ipython</span><span class="p">)</span></div></pre></div>
          </td>
        </tr>
      </table>
  </div>

          </div>
        </div>

        <a href="#jump-to-line" rel="facebox" data-hotkey="l" class="js-jump-to-line" style="display:none">Jump to Line</a>
        <div id="jump-to-line" style="display:none">
          <h2>Jump to Line</h2>
          <form accept-charset="UTF-8" class="js-jump-to-line-form">
            <input class="textfield js-jump-to-line-field" type="text">
            <div class="full-button">
              <button type="submit" class="button">Go</button>
            </div>
          </form>
        </div>

      </div>
    </div>
</div>

<div id="js-frame-loading-template" class="frame frame-loading large-loading-area" style="display:none;">
  <img class="js-frame-loading-spinner" src="https://a248.e.akamai.net/assets.github.com/images/spinners/octocat-spinner-128.gif?1359500886" height="64" width="64">
</div>


        </div>
      </div>
      <div class="context-overlay"></div>
    </div>

      <div id="footer-push"></div><!-- hack for sticky footer -->
    </div><!-- end of wrapper - hack for sticky footer -->

      <!-- footer -->
      <div id="footer">
  <div class="container clearfix">

      <dl class="footer_nav">
        <dt>GitHub</dt>
        <dd><a href="https://github.com/about">About us</a></dd>
        <dd><a href="https://github.com/blog">Blog</a></dd>
        <dd><a href="https://github.com/contact">Contact &amp; support</a></dd>
        <dd><a href="http://enterprise.github.com/">GitHub Enterprise</a></dd>
        <dd><a href="http://status.github.com/">Site status</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>Applications</dt>
        <dd><a href="http://mac.github.com/">GitHub for Mac</a></dd>
        <dd><a href="http://windows.github.com/">GitHub for Windows</a></dd>
        <dd><a href="http://eclipse.github.com/">GitHub for Eclipse</a></dd>
        <dd><a href="http://mobile.github.com/">GitHub mobile apps</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>Services</dt>
        <dd><a href="http://get.gaug.es/">Gauges: Web analytics</a></dd>
        <dd><a href="http://speakerdeck.com">Speaker Deck: Presentations</a></dd>
        <dd><a href="https://gist.github.com">Gist: Code snippets</a></dd>
        <dd><a href="http://jobs.github.com/">Job board</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>Documentation</dt>
        <dd><a href="http://help.github.com/">GitHub Help</a></dd>
        <dd><a href="http://developer.github.com/">Developer API</a></dd>
        <dd><a href="http://github.github.com/github-flavored-markdown/">GitHub Flavored Markdown</a></dd>
        <dd><a href="http://pages.github.com/">GitHub Pages</a></dd>
      </dl>

      <dl class="footer_nav">
        <dt>More</dt>
        <dd><a href="http://training.github.com/">Training</a></dd>
        <dd><a href="https://github.com/edu">Students &amp; teachers</a></dd>
        <dd><a href="http://shop.github.com">The Shop</a></dd>
        <dd><a href="/plans">Plans &amp; pricing</a></dd>
        <dd><a href="http://octodex.github.com/">The Octodex</a></dd>
      </dl>

      <hr class="footer-divider">


    <p class="right">&copy; 2013 <span title="0.05031s from fe19.rs.github.com">GitHub</span>, Inc. All rights reserved.</p>
    <a class="left" href="https://github.com/">
      <span class="mega-icon mega-icon-invertocat"></span>
    </a>
    <ul id="legal">
        <li><a href="https://github.com/site/terms">Terms of Service</a></li>
        <li><a href="https://github.com/site/privacy">Privacy</a></li>
        <li><a href="https://github.com/security">Security</a></li>
    </ul>

  </div><!-- /.container -->

</div><!-- /.#footer -->


    <div class="fullscreen-overlay js-fullscreen-overlay" id="fullscreen_overlay">
  <div class="fullscreen-container js-fullscreen-container">
    <div class="textarea-wrap">
      <textarea name="fullscreen-contents" id="fullscreen-contents" class="js-fullscreen-contents" placeholder="" data-suggester="fullscreen_suggester"></textarea>
          <div class="suggester-container">
              <div class="suggester fullscreen-suggester js-navigation-container" id="fullscreen_suggester"
                 data-url="/ipython/ipython/suggestions/commit">
              </div>
          </div>
    </div>
  </div>
  <div class="fullscreen-sidebar">
    <a href="#" class="exit-fullscreen js-exit-fullscreen tooltipped leftwards" title="Exit Zen Mode">
      <span class="mega-icon mega-icon-normalscreen"></span>
    </a>
    <a href="#" class="theme-switcher js-theme-switcher tooltipped leftwards"
      title="Switch themes">
      <span class="mini-icon mini-icon-brightness"></span>
    </a>
  </div>
</div>



    <div id="ajax-error-message" class="flash flash-error">
      <span class="mini-icon mini-icon-exclamation"></span>
      Something went wrong with that request. Please try again.
      <a href="#" class="mini-icon mini-icon-remove-close ajax-error-dismiss"></a>
    </div>

    
    
    <span id='server_response_time' data-time='0.05074' data-host='fe19'></span>
    
  </body>
</html>

