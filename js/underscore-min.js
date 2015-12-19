



<!DOCTYPE html>
<html lang="en" class="">
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# object: http://ogp.me/ns/object# article: http://ogp.me/ns/article# profile: http://ogp.me/ns/profile#">
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta http-equiv="Content-Language" content="en">
    <meta name="viewport" content="width=1020">
    
    
    <title>js-sequence-diagrams/underscore-min.js at gh-pages · bramp/js-sequence-diagrams · GitHub</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
    <link rel="apple-touch-icon" sizes="57x57" href="/apple-touch-icon-114.png">
    <link rel="apple-touch-icon" sizes="114x114" href="/apple-touch-icon-114.png">
    <link rel="apple-touch-icon" sizes="72x72" href="/apple-touch-icon-144.png">
    <link rel="apple-touch-icon" sizes="144x144" href="/apple-touch-icon-144.png">
    <meta property="fb:app_id" content="1401488693436528">

      <meta content="@github" name="twitter:site" /><meta content="summary" name="twitter:card" /><meta content="bramp/js-sequence-diagrams" name="twitter:title" /><meta content="js-sequence-diagrams - Draws simple SVG sequence diagrams from textual representation of the diagram" name="twitter:description" /><meta content="https://avatars0.githubusercontent.com/u/160627?v=3&amp;s=400" name="twitter:image:src" />
      <meta content="GitHub" property="og:site_name" /><meta content="object" property="og:type" /><meta content="https://avatars0.githubusercontent.com/u/160627?v=3&amp;s=400" property="og:image" /><meta content="bramp/js-sequence-diagrams" property="og:title" /><meta content="https://github.com/bramp/js-sequence-diagrams" property="og:url" /><meta content="js-sequence-diagrams - Draws simple SVG sequence diagrams from textual representation of the diagram" property="og:description" />
      <meta name="browser-stats-url" content="https://api.github.com/_private/browser/stats">
    <meta name="browser-errors-url" content="https://api.github.com/_private/browser/errors">
    <link rel="assets" href="https://assets-cdn.github.com/">
    
    <meta name="pjax-timeout" content="1000">
    

    <meta name="msapplication-TileImage" content="/windows-tile.png">
    <meta name="msapplication-TileColor" content="#ffffff">
    <meta name="selected-link" value="repo_source" data-pjax-transient>

    <meta name="google-site-verification" content="KT5gs8h0wvaagLKAVWq8bbeNwnZZK1r1XQysX3xurLU">
    <meta name="google-analytics" content="UA-3769691-2">

<meta content="collector.githubapp.com" name="octolytics-host" /><meta content="github" name="octolytics-app-id" /><meta content="CB9CE970:6922:1579C8E9:5674C668" name="octolytics-dimension-request_id" />
<meta content="/&lt;user-name&gt;/&lt;repo-name&gt;/blob/show" data-pjax-transient="true" name="analytics-location" />
<meta content="Rails, view, blob#show" data-pjax-transient="true" name="analytics-event" />


  <meta class="js-ga-set" name="dimension1" content="Logged Out">



        <meta name="hostname" content="github.com">
    <meta name="user-login" content="">

        <meta name="expected-hostname" content="github.com">

      <link rel="mask-icon" href="https://assets-cdn.github.com/pinned-octocat.svg" color="#4078c0">
      <link rel="icon" type="image/x-icon" href="https://assets-cdn.github.com/favicon.ico">

    <meta content="84b6269bfbafe41cbbca060618063ec532644322" name="form-nonce" />

    <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github-0b04211b92fadaf54a886f458f6b91c30af15dc4d01fdd7a5df4aaa37e11e691.css" media="all" rel="stylesheet" />
    <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github2-327ccd956b7110b749a5feef7e1110bd8a7f0074044affc068d39ff982377ae0.css" media="all" rel="stylesheet" />
    
    


    <meta http-equiv="x-pjax-version" content="a204374d75309b5a661fe6966cd2bea1">

      
  <meta name="description" content="js-sequence-diagrams - Draws simple SVG sequence diagrams from textual representation of the diagram">
  <meta name="go-import" content="github.com/bramp/js-sequence-diagrams git https://github.com/bramp/js-sequence-diagrams.git">

  <meta content="160627" name="octolytics-dimension-user_id" /><meta content="bramp" name="octolytics-dimension-user_login" /><meta content="6736646" name="octolytics-dimension-repository_id" /><meta content="bramp/js-sequence-diagrams" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="6736646" name="octolytics-dimension-repository_network_root_id" /><meta content="bramp/js-sequence-diagrams" name="octolytics-dimension-repository_network_root_nwo" />
  <link href="https://github.com/bramp/js-sequence-diagrams/commits/gh-pages.atom" rel="alternate" title="Recent Commits to js-sequence-diagrams:gh-pages" type="application/atom+xml">

  </head>


  <body class="logged_out   env-production  vis-public page-blob">
    <a href="#start-of-content" tabindex="1" class="accessibility-aid js-skip-to-content">Skip to content</a>

    
    
    



      
      <div class="header header-logged-out" role="banner">
  <div class="container clearfix">

    <a class="header-logo-wordmark" href="https://github.com/" data-ga-click="(Logged out) Header, go to homepage, icon:logo-wordmark">
      <span class="mega-octicon octicon-logo-github"></span>
    </a>

    <div class="header-actions" role="navigation">
        <a class="btn btn-primary" href="/join" data-ga-click="(Logged out) Header, clicked Sign up, text:sign-up">Sign up</a>
      <a class="btn" href="/login?return_to=%2Fbramp%2Fjs-sequence-diagrams%2Fblob%2Fgh-pages%2Fjs%2Funderscore-min.js" data-ga-click="(Logged out) Header, clicked Sign in, text:sign-in">Sign in</a>
    </div>

    <div class="site-search repo-scope js-site-search" role="search">
      <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/bramp/js-sequence-diagrams/search" class="js-site-search-form" data-global-search-url="/search" data-repo-search-url="/bramp/js-sequence-diagrams/search" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
  <label class="js-chromeless-input-container form-control">
    <div class="scope-badge">This repository</div>
    <input type="text"
      class="js-site-search-focus js-site-search-field is-clearable chromeless-input"
      data-hotkey="s"
      name="q"
      placeholder="Search"
      aria-label="Search this repository"
      data-global-scope-placeholder="Search GitHub"
      data-repo-scope-placeholder="Search"
      tabindex="1"
      autocapitalize="off">
  </label>
</form>
    </div>

      <ul class="header-nav left" role="navigation">
          <li class="header-nav-item">
            <a class="header-nav-link" href="/explore" data-ga-click="(Logged out) Header, go to explore, text:explore">Explore</a>
          </li>
          <li class="header-nav-item">
            <a class="header-nav-link" href="/features" data-ga-click="(Logged out) Header, go to features, text:features">Features</a>
          </li>
          <li class="header-nav-item">
            <a class="header-nav-link" href="https://enterprise.github.com/" data-ga-click="(Logged out) Header, go to enterprise, text:enterprise">Enterprise</a>
          </li>
          <li class="header-nav-item">
            <a class="header-nav-link" href="/pricing" data-ga-click="(Logged out) Header, go to pricing, text:pricing">Pricing</a>
          </li>
      </ul>

  </div>
</div>



    <div id="start-of-content" class="accessibility-aid"></div>

      <div id="js-flash-container">
</div>


    <div role="main" class="main-content">
        <div itemscope itemtype="http://schema.org/WebPage">
    <div id="js-repo-pjax-container" class="context-loader-container js-repo-nav-next" data-pjax-container>
      
<div class="pagehead repohead instapaper_ignore readability-menu experiment-repo-nav">
  <div class="container repohead-details-container">

    

<ul class="pagehead-actions">

  <li>
      <a href="/login?return_to=%2Fbramp%2Fjs-sequence-diagrams"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to watch a repository" rel="nofollow">
    <span class="octicon octicon-eye"></span>
    Watch
  </a>
  <a class="social-count" href="/bramp/js-sequence-diagrams/watchers">
    186
  </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fbramp%2Fjs-sequence-diagrams"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to star a repository" rel="nofollow">
    <span class="octicon octicon-star "></span>
    Star
  </a>

    <a class="social-count js-social-count" href="/bramp/js-sequence-diagrams/stargazers">
      3,656
    </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fbramp%2Fjs-sequence-diagrams"
        class="btn btn-sm btn-with-count tooltipped tooltipped-n"
        aria-label="You must be signed in to fork a repository" rel="nofollow">
        <span class="octicon octicon-repo-forked "></span>
        Fork
      </a>

    <a href="/bramp/js-sequence-diagrams/network" class="social-count">
      448
    </a>
  </li>
</ul>

    <h1 itemscope itemtype="http://data-vocabulary.org/Breadcrumb" class="entry-title public ">
  <span class="octicon octicon-repo "></span>
  <span class="author"><a href="/bramp" class="url fn" itemprop="url" rel="author"><span itemprop="title">bramp</span></a></span><!--
--><span class="path-divider">/</span><!--
--><strong><a href="/bramp/js-sequence-diagrams" data-pjax="#js-repo-pjax-container">js-sequence-diagrams</a></strong>

  <span class="page-context-loader">
    <img alt="" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32.gif" width="16" />
  </span>

</h1>

  </div>
  <div class="container">
    
<nav class="reponav js-repo-nav js-sidenav-container-pjax js-octicon-loaders"
     role="navigation"
     data-pjax="#js-repo-pjax-container">

  <a href="/bramp/js-sequence-diagrams/tree/gh-pages" aria-label="Code" aria-selected="true" class="js-selected-navigation-item selected reponav-item" data-hotkey="g c" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches /bramp/js-sequence-diagrams/tree/gh-pages">
    <span class="octicon octicon-code "></span>
    Code
</a>
    <a href="/bramp/js-sequence-diagrams/issues" class="js-selected-navigation-item reponav-item" data-hotkey="g i" data-selected-links="repo_issues repo_labels repo_milestones /bramp/js-sequence-diagrams/issues">
      <span class="octicon octicon-issue-opened "></span>
      Issues
      <span class="counter">51</span>
</a>
  <a href="/bramp/js-sequence-diagrams/pulls" class="js-selected-navigation-item reponav-item" data-hotkey="g p" data-selected-links="repo_pulls /bramp/js-sequence-diagrams/pulls">
    <span class="octicon octicon-git-pull-request "></span>
    Pull requests
    <span class="counter">8</span>
</a>

  <a href="/bramp/js-sequence-diagrams/pulse" class="js-selected-navigation-item reponav-item" data-selected-links="pulse /bramp/js-sequence-diagrams/pulse">
    <span class="octicon octicon-pulse "></span>
    Pulse
</a>
  <a href="/bramp/js-sequence-diagrams/graphs" class="js-selected-navigation-item reponav-item" data-selected-links="repo_graphs repo_contributors /bramp/js-sequence-diagrams/graphs">
    <span class="octicon octicon-graph "></span>
    Graphs
</a>

</nav>

  </div>
</div>

<div class="container new-discussion-timeline experiment-repo-nav">
  <div class="repository-content">

    

<a href="/bramp/js-sequence-diagrams/blob/e4acd26e156bd12c6bb49d5c13e0ae3fddac63c3/js/underscore-min.js" class="hidden js-permalink-shortcut" data-hotkey="y">Permalink</a>

<!-- blob contrib key: blob_contributors:v21:8f910c32d19de1b485611678873cae60 -->

<div class="file-navigation js-zeroclipboard-container">
  
<div class="select-menu js-menu-container js-select-menu left">
  <button class="btn btn-sm select-menu-button js-menu-target css-truncate" data-hotkey="w"
    title="gh-pages"
    type="button" aria-label="Switch branches or tags" tabindex="0" aria-haspopup="true">
    <i>Branch:</i>
    <span class="js-select-button css-truncate-target">gh-pages</span>
  </button>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax aria-hidden="true">

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <span aria-label="Close" class="octicon octicon-x js-menu-close" role="button"></span>
        <span class="select-menu-title">Switch branches/tags</span>
      </div>

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Filter branches/tags" id="context-commitish-filter-field" class="js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" data-filter-placeholder="Filter branches/tags" class="js-select-menu-tab" role="tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" data-filter-placeholder="Find a tag…" class="js-select-menu-tab" role="tab">Tags</a>
            </li>
          </ul>
        </div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches" role="menu">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open selected"
               href="/bramp/js-sequence-diagrams/blob/gh-pages/js/underscore-min.js"
               data-name="gh-pages"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="gh-pages">
                gh-pages
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/bramp/js-sequence-diagrams/blob/master/js/underscore-min.js"
               data-name="master"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="master">
                master
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/bramp/js-sequence-diagrams/blob/newfont/js/underscore-min.js"
               data-name="newfont"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="newfont">
                newfont
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/bramp/js-sequence-diagrams/blob/svg/js/underscore-min.js"
               data-name="svg"
               data-skip-pjax="true"
               rel="nofollow">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <span class="select-menu-item-text css-truncate-target" title="svg">
                svg
              </span>
            </a>
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/bramp/js-sequence-diagrams/tree/v1.0.6/js/underscore-min.js"
                 data-name="v1.0.6"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="v1.0.6">v1.0.6</a>
            </div>
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/bramp/js-sequence-diagrams/tree/v1.0.5/js/underscore-min.js"
                 data-name="v1.0.5"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="v1.0.5">v1.0.5</a>
            </div>
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/bramp/js-sequence-diagrams/tree/v1.0.4/js/underscore-min.js"
                 data-name="v1.0.4"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="v1.0.4">v1.0.4</a>
            </div>
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/bramp/js-sequence-diagrams/tree/v1.0.3/js/underscore-min.js"
                 data-name="v1.0.3"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="v1.0.3">v1.0.3</a>
            </div>
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/bramp/js-sequence-diagrams/tree/v1.0.2/js/underscore-min.js"
                 data-name="v1.0.2"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="v1.0.2">v1.0.2</a>
            </div>
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/bramp/js-sequence-diagrams/tree/v1.0.1/js/underscore-min.js"
                 data-name="v1.0.1"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="v1.0.1">v1.0.1</a>
            </div>
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/bramp/js-sequence-diagrams/tree/v1.0/js/underscore-min.js"
                 data-name="v1.0"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text css-truncate-target"
                 title="v1.0">v1.0</a>
            </div>
        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div>

    </div>
  </div>
</div>

  <div class="btn-group right">
    <a href="/bramp/js-sequence-diagrams/find/gh-pages"
          class="js-show-file-finder btn btn-sm"
          data-pjax
          data-hotkey="t">
      Find file
    </a>
    <button aria-label="Copy file path to clipboard" class="js-zeroclipboard btn btn-sm zeroclipboard-button tooltipped tooltipped-s" data-copied-hint="Copied!" type="button">Copy path</button>
  </div>
  <div class="breadcrumb js-zeroclipboard-target">
    <span class="repo-root js-repo-root"><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/bramp/js-sequence-diagrams/tree/gh-pages" class="" data-branch="gh-pages" data-pjax="true" itemscope="url"><span itemprop="title">js-sequence-diagrams</span></a></span></span><span class="separator">/</span><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/bramp/js-sequence-diagrams/tree/gh-pages/js" class="" data-branch="gh-pages" data-pjax="true" itemscope="url"><span itemprop="title">js</span></a></span><span class="separator">/</span><strong class="final-path">underscore-min.js</strong>
  </div>
</div>

<include-fragment class="commit-tease" src="/bramp/js-sequence-diagrams/contributors/gh-pages/js/underscore-min.js">
  <div>
    Fetching contributors&hellip;
  </div>

  <div class="commit-tease-contributors">
    <img alt="" class="loader-loading left" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32-EAF2F5.gif" width="16" />
    <span class="loader-error">Cannot retrieve contributors at this time</span>
  </div>
</include-fragment>
<div class="file">
  <div class="file-header">
  <div class="file-actions">

    <div class="btn-group">
      <a href="/bramp/js-sequence-diagrams/raw/gh-pages/js/underscore-min.js" class="btn btn-sm " id="raw-url">Raw</a>
        <a href="/bramp/js-sequence-diagrams/blame/gh-pages/js/underscore-min.js" class="btn btn-sm js-update-url-with-hash">Blame</a>
      <a href="/bramp/js-sequence-diagrams/commits/gh-pages/js/underscore-min.js" class="btn btn-sm " rel="nofollow">History</a>
    </div>


        <button type="button" class="octicon-btn disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <span class="octicon octicon-pencil"></span>
        </button>
        <button type="button" class="octicon-btn octicon-btn-danger disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <span class="octicon octicon-trashcan"></span>
        </button>
  </div>

  <div class="file-info">
      1 lines (1 sloc)
      <span class="file-info-divider"></span>
    13.1 KB
  </div>
</div>

  

  <div class="blob-wrapper data type-javascript">
      <table class="highlight tab-size js-file-line-container" data-tab-size="8">
      <tr>
        <td id="L1" class="blob-num js-line-number" data-line-number="1"></td>
        <td id="LC1" class="blob-code blob-code-inner js-file-line">(function(){var n=this,t=n._,r={},e=Array.prototype,u=Object.prototype,i=Function.prototype,a=e.push,o=e.slice,c=e.concat,l=u.toString,f=u.hasOwnProperty,s=e.forEach,p=e.map,h=e.reduce,v=e.reduceRight,d=e.filter,g=e.every,m=e.some,y=e.indexOf,b=e.lastIndexOf,x=Array.isArray,_=Object.keys,j=i.bind,w=function(n){return n instanceof w?n:this instanceof w?(this._wrapped=n,void 0):new w(n)};&quot;undefined&quot;!=typeof exports?(&quot;undefined&quot;!=typeof module&amp;&amp;module.exports&amp;&amp;(exports=module.exports=w),exports._=w):n._=w,w.VERSION=&quot;1.4.4&quot;;var A=w.each=w.forEach=function(n,t,e){if(null!=n)if(s&amp;&amp;n.forEach===s)n.forEach(t,e);else if(n.length===+n.length){for(var u=0,i=n.length;i&gt;u;u++)if(t.call(e,n[u],u,n)===r)return}else for(var a in n)if(w.has(n,a)&amp;&amp;t.call(e,n[a],a,n)===r)return};w.map=w.collect=function(n,t,r){var e=[];return null==n?e:p&amp;&amp;n.map===p?n.map(t,r):(A(n,function(n,u,i){e[e.length]=t.call(r,n,u,i)}),e)};var O=&quot;Reduce of empty array with no initial value&quot;;w.reduce=w.foldl=w.inject=function(n,t,r,e){var u=arguments.length&gt;2;if(null==n&amp;&amp;(n=[]),h&amp;&amp;n.reduce===h)return e&amp;&amp;(t=w.bind(t,e)),u?n.reduce(t,r):n.reduce(t);if(A(n,function(n,i,a){u?r=t.call(e,r,n,i,a):(r=n,u=!0)}),!u)throw new TypeError(O);return r},w.reduceRight=w.foldr=function(n,t,r,e){var u=arguments.length&gt;2;if(null==n&amp;&amp;(n=[]),v&amp;&amp;n.reduceRight===v)return e&amp;&amp;(t=w.bind(t,e)),u?n.reduceRight(t,r):n.reduceRight(t);var i=n.length;if(i!==+i){var a=w.keys(n);i=a.length}if(A(n,function(o,c,l){c=a?a[--i]:--i,u?r=t.call(e,r,n[c],c,l):(r=n[c],u=!0)}),!u)throw new TypeError(O);return r},w.find=w.detect=function(n,t,r){var e;return E(n,function(n,u,i){return t.call(r,n,u,i)?(e=n,!0):void 0}),e},w.filter=w.select=function(n,t,r){var e=[];return null==n?e:d&amp;&amp;n.filter===d?n.filter(t,r):(A(n,function(n,u,i){t.call(r,n,u,i)&amp;&amp;(e[e.length]=n)}),e)},w.reject=function(n,t,r){return w.filter(n,function(n,e,u){return!t.call(r,n,e,u)},r)},w.every=w.all=function(n,t,e){t||(t=w.identity);var u=!0;return null==n?u:g&amp;&amp;n.every===g?n.every(t,e):(A(n,function(n,i,a){return(u=u&amp;&amp;t.call(e,n,i,a))?void 0:r}),!!u)};var E=w.some=w.any=function(n,t,e){t||(t=w.identity);var u=!1;return null==n?u:m&amp;&amp;n.some===m?n.some(t,e):(A(n,function(n,i,a){return u||(u=t.call(e,n,i,a))?r:void 0}),!!u)};w.contains=w.include=function(n,t){return null==n?!1:y&amp;&amp;n.indexOf===y?n.indexOf(t)!=-1:E(n,function(n){return n===t})},w.invoke=function(n,t){var r=o.call(arguments,2),e=w.isFunction(t);return w.map(n,function(n){return(e?t:n[t]).apply(n,r)})},w.pluck=function(n,t){return w.map(n,function(n){return n[t]})},w.where=function(n,t,r){return w.isEmpty(t)?r?null:[]:w[r?&quot;find&quot;:&quot;filter&quot;](n,function(n){for(var r in t)if(t[r]!==n[r])return!1;return!0})},w.findWhere=function(n,t){return w.where(n,t,!0)},w.max=function(n,t,r){if(!t&amp;&amp;w.isArray(n)&amp;&amp;n[0]===+n[0]&amp;&amp;65535&gt;n.length)return Math.max.apply(Math,n);if(!t&amp;&amp;w.isEmpty(n))return-1/0;var e={computed:-1/0,value:-1/0};return A(n,function(n,u,i){var a=t?t.call(r,n,u,i):n;a&gt;=e.computed&amp;&amp;(e={value:n,computed:a})}),e.value},w.min=function(n,t,r){if(!t&amp;&amp;w.isArray(n)&amp;&amp;n[0]===+n[0]&amp;&amp;65535&gt;n.length)return Math.min.apply(Math,n);if(!t&amp;&amp;w.isEmpty(n))return 1/0;var e={computed:1/0,value:1/0};return A(n,function(n,u,i){var a=t?t.call(r,n,u,i):n;e.computed&gt;a&amp;&amp;(e={value:n,computed:a})}),e.value},w.shuffle=function(n){var t,r=0,e=[];return A(n,function(n){t=w.random(r++),e[r-1]=e[t],e[t]=n}),e};var k=function(n){return w.isFunction(n)?n:function(t){return t[n]}};w.sortBy=function(n,t,r){var e=k(t);return w.pluck(w.map(n,function(n,t,u){return{value:n,index:t,criteria:e.call(r,n,t,u)}}).sort(function(n,t){var r=n.criteria,e=t.criteria;if(r!==e){if(r&gt;e||r===void 0)return 1;if(e&gt;r||e===void 0)return-1}return n.index&lt;t.index?-1:1}),&quot;value&quot;)};var F=function(n,t,r,e){var u={},i=k(t||w.identity);return A(n,function(t,a){var o=i.call(r,t,a,n);e(u,o,t)}),u};w.groupBy=function(n,t,r){return F(n,t,r,function(n,t,r){(w.has(n,t)?n[t]:n[t]=[]).push(r)})},w.countBy=function(n,t,r){return F(n,t,r,function(n,t){w.has(n,t)||(n[t]=0),n[t]++})},w.sortedIndex=function(n,t,r,e){r=null==r?w.identity:k(r);for(var u=r.call(e,t),i=0,a=n.length;a&gt;i;){var o=i+a&gt;&gt;&gt;1;u&gt;r.call(e,n[o])?i=o+1:a=o}return i},w.toArray=function(n){return n?w.isArray(n)?o.call(n):n.length===+n.length?w.map(n,w.identity):w.values(n):[]},w.size=function(n){return null==n?0:n.length===+n.length?n.length:w.keys(n).length},w.first=w.head=w.take=function(n,t,r){return null==n?void 0:null==t||r?n[0]:o.call(n,0,t)},w.initial=function(n,t,r){return o.call(n,0,n.length-(null==t||r?1:t))},w.last=function(n,t,r){return null==n?void 0:null==t||r?n[n.length-1]:o.call(n,Math.max(n.length-t,0))},w.rest=w.tail=w.drop=function(n,t,r){return o.call(n,null==t||r?1:t)},w.compact=function(n){return w.filter(n,w.identity)};var R=function(n,t,r){return A(n,function(n){w.isArray(n)?t?a.apply(r,n):R(n,t,r):r.push(n)}),r};w.flatten=function(n,t){return R(n,t,[])},w.without=function(n){return w.difference(n,o.call(arguments,1))},w.uniq=w.unique=function(n,t,r,e){w.isFunction(t)&amp;&amp;(e=r,r=t,t=!1);var u=r?w.map(n,r,e):n,i=[],a=[];return A(u,function(r,e){(t?e&amp;&amp;a[a.length-1]===r:w.contains(a,r))||(a.push(r),i.push(n[e]))}),i},w.union=function(){return w.uniq(c.apply(e,arguments))},w.intersection=function(n){var t=o.call(arguments,1);return w.filter(w.uniq(n),function(n){return w.every(t,function(t){return w.indexOf(t,n)&gt;=0})})},w.difference=function(n){var t=c.apply(e,o.call(arguments,1));return w.filter(n,function(n){return!w.contains(t,n)})},w.zip=function(){for(var n=o.call(arguments),t=w.max(w.pluck(n,&quot;length&quot;)),r=Array(t),e=0;t&gt;e;e++)r[e]=w.pluck(n,&quot;&quot;+e);return r},w.object=function(n,t){if(null==n)return{};for(var r={},e=0,u=n.length;u&gt;e;e++)t?r[n[e]]=t[e]:r[n[e][0]]=n[e][1];return r},w.indexOf=function(n,t,r){if(null==n)return-1;var e=0,u=n.length;if(r){if(&quot;number&quot;!=typeof r)return e=w.sortedIndex(n,t),n[e]===t?e:-1;e=0&gt;r?Math.max(0,u+r):r}if(y&amp;&amp;n.indexOf===y)return n.indexOf(t,r);for(;u&gt;e;e++)if(n[e]===t)return e;return-1},w.lastIndexOf=function(n,t,r){if(null==n)return-1;var e=null!=r;if(b&amp;&amp;n.lastIndexOf===b)return e?n.lastIndexOf(t,r):n.lastIndexOf(t);for(var u=e?r:n.length;u--;)if(n[u]===t)return u;return-1},w.range=function(n,t,r){1&gt;=arguments.length&amp;&amp;(t=n||0,n=0),r=arguments[2]||1;for(var e=Math.max(Math.ceil((t-n)/r),0),u=0,i=Array(e);e&gt;u;)i[u++]=n,n+=r;return i},w.bind=function(n,t){if(n.bind===j&amp;&amp;j)return j.apply(n,o.call(arguments,1));var r=o.call(arguments,2);return function(){return n.apply(t,r.concat(o.call(arguments)))}},w.partial=function(n){var t=o.call(arguments,1);return function(){return n.apply(this,t.concat(o.call(arguments)))}},w.bindAll=function(n){var t=o.call(arguments,1);return 0===t.length&amp;&amp;(t=w.functions(n)),A(t,function(t){n[t]=w.bind(n[t],n)}),n},w.memoize=function(n,t){var r={};return t||(t=w.identity),function(){var e=t.apply(this,arguments);return w.has(r,e)?r[e]:r[e]=n.apply(this,arguments)}},w.delay=function(n,t){var r=o.call(arguments,2);return setTimeout(function(){return n.apply(null,r)},t)},w.defer=function(n){return w.delay.apply(w,[n,1].concat(o.call(arguments,1)))},w.throttle=function(n,t){var r,e,u,i,a=0,o=function(){a=new Date,u=null,i=n.apply(r,e)};return function(){var c=new Date,l=t-(c-a);return r=this,e=arguments,0&gt;=l?(clearTimeout(u),u=null,a=c,i=n.apply(r,e)):u||(u=setTimeout(o,l)),i}},w.debounce=function(n,t,r){var e,u;return function(){var i=this,a=arguments,o=function(){e=null,r||(u=n.apply(i,a))},c=r&amp;&amp;!e;return clearTimeout(e),e=setTimeout(o,t),c&amp;&amp;(u=n.apply(i,a)),u}},w.once=function(n){var t,r=!1;return function(){return r?t:(r=!0,t=n.apply(this,arguments),n=null,t)}},w.wrap=function(n,t){return function(){var r=[n];return a.apply(r,arguments),t.apply(this,r)}},w.compose=function(){var n=arguments;return function(){for(var t=arguments,r=n.length-1;r&gt;=0;r--)t=[n[r].apply(this,t)];return t[0]}},w.after=function(n,t){return 0&gt;=n?t():function(){return 1&gt;--n?t.apply(this,arguments):void 0}},w.keys=_||function(n){if(n!==Object(n))throw new TypeError(&quot;Invalid object&quot;);var t=[];for(var r in n)w.has(n,r)&amp;&amp;(t[t.length]=r);return t},w.values=function(n){var t=[];for(var r in n)w.has(n,r)&amp;&amp;t.push(n[r]);return t},w.pairs=function(n){var t=[];for(var r in n)w.has(n,r)&amp;&amp;t.push([r,n[r]]);return t},w.invert=function(n){var t={};for(var r in n)w.has(n,r)&amp;&amp;(t[n[r]]=r);return t},w.functions=w.methods=function(n){var t=[];for(var r in n)w.isFunction(n[r])&amp;&amp;t.push(r);return t.sort()},w.extend=function(n){return A(o.call(arguments,1),function(t){if(t)for(var r in t)n[r]=t[r]}),n},w.pick=function(n){var t={},r=c.apply(e,o.call(arguments,1));return A(r,function(r){r in n&amp;&amp;(t[r]=n[r])}),t},w.omit=function(n){var t={},r=c.apply(e,o.call(arguments,1));for(var u in n)w.contains(r,u)||(t[u]=n[u]);return t},w.defaults=function(n){return A(o.call(arguments,1),function(t){if(t)for(var r in t)null==n[r]&amp;&amp;(n[r]=t[r])}),n},w.clone=function(n){return w.isObject(n)?w.isArray(n)?n.slice():w.extend({},n):n},w.tap=function(n,t){return t(n),n};var I=function(n,t,r,e){if(n===t)return 0!==n||1/n==1/t;if(null==n||null==t)return n===t;n instanceof w&amp;&amp;(n=n._wrapped),t instanceof w&amp;&amp;(t=t._wrapped);var u=l.call(n);if(u!=l.call(t))return!1;switch(u){case&quot;[object String]&quot;:return n==t+&quot;&quot;;case&quot;[object Number]&quot;:return n!=+n?t!=+t:0==n?1/n==1/t:n==+t;case&quot;[object Date]&quot;:case&quot;[object Boolean]&quot;:return+n==+t;case&quot;[object RegExp]&quot;:return n.source==t.source&amp;&amp;n.global==t.global&amp;&amp;n.multiline==t.multiline&amp;&amp;n.ignoreCase==t.ignoreCase}if(&quot;object&quot;!=typeof n||&quot;object&quot;!=typeof t)return!1;for(var i=r.length;i--;)if(r[i]==n)return e[i]==t;r.push(n),e.push(t);var a=0,o=!0;if(&quot;[object Array]&quot;==u){if(a=n.length,o=a==t.length)for(;a--&amp;&amp;(o=I(n[a],t[a],r,e)););}else{var c=n.constructor,f=t.constructor;if(c!==f&amp;&amp;!(w.isFunction(c)&amp;&amp;c instanceof c&amp;&amp;w.isFunction(f)&amp;&amp;f instanceof f))return!1;for(var s in n)if(w.has(n,s)&amp;&amp;(a++,!(o=w.has(t,s)&amp;&amp;I(n[s],t[s],r,e))))break;if(o){for(s in t)if(w.has(t,s)&amp;&amp;!a--)break;o=!a}}return r.pop(),e.pop(),o};w.isEqual=function(n,t){return I(n,t,[],[])},w.isEmpty=function(n){if(null==n)return!0;if(w.isArray(n)||w.isString(n))return 0===n.length;for(var t in n)if(w.has(n,t))return!1;return!0},w.isElement=function(n){return!(!n||1!==n.nodeType)},w.isArray=x||function(n){return&quot;[object Array]&quot;==l.call(n)},w.isObject=function(n){return n===Object(n)},A([&quot;Arguments&quot;,&quot;Function&quot;,&quot;String&quot;,&quot;Number&quot;,&quot;Date&quot;,&quot;RegExp&quot;],function(n){w[&quot;is&quot;+n]=function(t){return l.call(t)==&quot;[object &quot;+n+&quot;]&quot;}}),w.isArguments(arguments)||(w.isArguments=function(n){return!(!n||!w.has(n,&quot;callee&quot;))}),&quot;function&quot;!=typeof/./&amp;&amp;(w.isFunction=function(n){return&quot;function&quot;==typeof n}),w.isFinite=function(n){return isFinite(n)&amp;&amp;!isNaN(parseFloat(n))},w.isNaN=function(n){return w.isNumber(n)&amp;&amp;n!=+n},w.isBoolean=function(n){return n===!0||n===!1||&quot;[object Boolean]&quot;==l.call(n)},w.isNull=function(n){return null===n},w.isUndefined=function(n){return n===void 0},w.has=function(n,t){return f.call(n,t)},w.noConflict=function(){return n._=t,this},w.identity=function(n){return n},w.times=function(n,t,r){for(var e=Array(n),u=0;n&gt;u;u++)e[u]=t.call(r,u);return e},w.random=function(n,t){return null==t&amp;&amp;(t=n,n=0),n+Math.floor(Math.random()*(t-n+1))};var M={escape:{&quot;&amp;&quot;:&quot;&amp;amp;&quot;,&quot;&lt;&quot;:&quot;&amp;lt;&quot;,&quot;&gt;&quot;:&quot;&amp;gt;&quot;,&#39;&quot;&#39;:&quot;&amp;quot;&quot;,&quot;&#39;&quot;:&quot;&amp;#x27;&quot;,&quot;/&quot;:&quot;&amp;#x2F;&quot;}};M.unescape=w.invert(M.escape);var S={escape:RegExp(&quot;[&quot;+w.keys(M.escape).join(&quot;&quot;)+&quot;]&quot;,&quot;g&quot;),unescape:RegExp(&quot;(&quot;+w.keys(M.unescape).join(&quot;|&quot;)+&quot;)&quot;,&quot;g&quot;)};w.each([&quot;escape&quot;,&quot;unescape&quot;],function(n){w[n]=function(t){return null==t?&quot;&quot;:(&quot;&quot;+t).replace(S[n],function(t){return M[n][t]})}}),w.result=function(n,t){if(null==n)return null;var r=n[t];return w.isFunction(r)?r.call(n):r},w.mixin=function(n){A(w.functions(n),function(t){var r=w[t]=n[t];w.prototype[t]=function(){var n=[this._wrapped];return a.apply(n,arguments),D.call(this,r.apply(w,n))}})};var N=0;w.uniqueId=function(n){var t=++N+&quot;&quot;;return n?n+t:t},w.templateSettings={evaluate:/&lt;%([\s\S]+?)%&gt;/g,interpolate:/&lt;%=([\s\S]+?)%&gt;/g,escape:/&lt;%-([\s\S]+?)%&gt;/g};var T=/(.)^/,q={&quot;&#39;&quot;:&quot;&#39;&quot;,&quot;\\&quot;:&quot;\\&quot;,&quot;\r&quot;:&quot;r&quot;,&quot;\n&quot;:&quot;n&quot;,&quot;	&quot;:&quot;t&quot;,&quot;\u2028&quot;:&quot;u2028&quot;,&quot;\u2029&quot;:&quot;u2029&quot;},B=/\\|&#39;|\r|\n|\t|\u2028|\u2029/g;w.template=function(n,t,r){var e;r=w.defaults({},r,w.templateSettings);var u=RegExp([(r.escape||T).source,(r.interpolate||T).source,(r.evaluate||T).source].join(&quot;|&quot;)+&quot;|$&quot;,&quot;g&quot;),i=0,a=&quot;__p+=&#39;&quot;;n.replace(u,function(t,r,e,u,o){return a+=n.slice(i,o).replace(B,function(n){return&quot;\\&quot;+q[n]}),r&amp;&amp;(a+=&quot;&#39;+\n((__t=(&quot;+r+&quot;))==null?&#39;&#39;:_.escape(__t))+\n&#39;&quot;),e&amp;&amp;(a+=&quot;&#39;+\n((__t=(&quot;+e+&quot;))==null?&#39;&#39;:__t)+\n&#39;&quot;),u&amp;&amp;(a+=&quot;&#39;;\n&quot;+u+&quot;\n__p+=&#39;&quot;),i=o+t.length,t}),a+=&quot;&#39;;\n&quot;,r.variable||(a=&quot;with(obj||{}){\n&quot;+a+&quot;}\n&quot;),a=&quot;var __t,__p=&#39;&#39;,__j=Array.prototype.join,&quot;+&quot;print=function(){__p+=__j.call(arguments,&#39;&#39;);};\n&quot;+a+&quot;return __p;\n&quot;;try{e=Function(r.variable||&quot;obj&quot;,&quot;_&quot;,a)}catch(o){throw o.source=a,o}if(t)return e(t,w);var c=function(n){return e.call(this,n,w)};return c.source=&quot;function(&quot;+(r.variable||&quot;obj&quot;)+&quot;){\n&quot;+a+&quot;}&quot;,c},w.chain=function(n){return w(n).chain()};var D=function(n){return this._chain?w(n).chain():n};w.mixin(w),A([&quot;pop&quot;,&quot;push&quot;,&quot;reverse&quot;,&quot;shift&quot;,&quot;sort&quot;,&quot;splice&quot;,&quot;unshift&quot;],function(n){var t=e[n];w.prototype[n]=function(){var r=this._wrapped;return t.apply(r,arguments),&quot;shift&quot;!=n&amp;&amp;&quot;splice&quot;!=n||0!==r.length||delete r[0],D.call(this,r)}}),A([&quot;concat&quot;,&quot;join&quot;,&quot;slice&quot;],function(n){var t=e[n];w.prototype[n]=function(){return D.call(this,t.apply(this._wrapped,arguments))}}),w.extend(w.prototype,{chain:function(){return this._chain=!0,this},value:function(){return this._wrapped}})}).call(this);</td>
      </tr>
</table>

  </div>

</div>

<a href="#jump-to-line" rel="facebox[.linejump]" data-hotkey="l" style="display:none">Jump to Line</a>
<div id="jump-to-line" style="display:none">
  <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="" class="js-jump-to-line-form" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
    <input class="linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" aria-label="Jump to line" autofocus>
    <button type="submit" class="btn">Go</button>
</form></div>

  </div>
  <div class="modal-backdrop"></div>
</div>

    </div>
  </div>

    </div>

        <div class="container">
  <div class="site-footer" role="contentinfo">
    <ul class="site-footer-links right">
        <li><a href="https://status.github.com/" data-ga-click="Footer, go to status, text:status">Status</a></li>
      <li><a href="https://developer.github.com" data-ga-click="Footer, go to api, text:api">API</a></li>
      <li><a href="https://training.github.com" data-ga-click="Footer, go to training, text:training">Training</a></li>
      <li><a href="https://shop.github.com" data-ga-click="Footer, go to shop, text:shop">Shop</a></li>
        <li><a href="https://github.com/blog" data-ga-click="Footer, go to blog, text:blog">Blog</a></li>
        <li><a href="https://github.com/about" data-ga-click="Footer, go to about, text:about">About</a></li>
        <li><a href="https://github.com/pricing" data-ga-click="Footer, go to pricing, text:pricing">Pricing</a></li>

    </ul>

    <a href="https://github.com" aria-label="Homepage">
      <span class="mega-octicon octicon-mark-github " title="GitHub "></span>
</a>
    <ul class="site-footer-links">
      <li>&copy; 2015 <span title="0.07657s from github-fe142-cp1-prd.iad.github.net">GitHub</span>, Inc.</li>
        <li><a href="https://github.com/site/terms" data-ga-click="Footer, go to terms, text:terms">Terms</a></li>
        <li><a href="https://github.com/site/privacy" data-ga-click="Footer, go to privacy, text:privacy">Privacy</a></li>
        <li><a href="https://github.com/security" data-ga-click="Footer, go to security, text:security">Security</a></li>
        <li><a href="https://github.com/contact" data-ga-click="Footer, go to contact, text:contact">Contact</a></li>
        <li><a href="https://help.github.com" data-ga-click="Footer, go to help, text:help">Help</a></li>
    </ul>
  </div>
</div>



    
    
    

    <div id="ajax-error-message" class="flash flash-error">
      <span class="octicon octicon-alert"></span>
      <button type="button" class="flash-close js-flash-close js-ajax-error-dismiss" aria-label="Dismiss error">
        <span class="octicon octicon-x"></span>
      </button>
      Something went wrong with that request. Please try again.
    </div>


      <script crossorigin="anonymous" src="https://assets-cdn.github.com/assets/frameworks-ef8eb4a89ee9f3c8b7613307fe589a8f5705817f7cee27bec51ce5e963234abf.js"></script>
      <script async="async" crossorigin="anonymous" src="https://assets-cdn.github.com/assets/github-4b6b8e7d11ebb7bce85126d3b4130c80041f2ae6d5d6ef8901a8c0c3f7cb80d2.js"></script>
      
      
      
    <div class="js-stale-session-flash stale-session-flash flash flash-warn flash-banner hidden">
      <span class="octicon octicon-alert"></span>
      <span class="signed-in-tab-flash">You signed in with another tab or window. <a href="">Reload</a> to refresh your session.</span>
      <span class="signed-out-tab-flash">You signed out in another tab or window. <a href="">Reload</a> to refresh your session.</span>
    </div>
  </body>
</html>

