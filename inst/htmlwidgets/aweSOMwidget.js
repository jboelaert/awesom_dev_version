////////////////////////////////////////////////////////////////////////////////
//// Function for hex cells
////////////////////////////////////////////////////////////////////////////////

d3.hexbin = function() {
  var width = 1,
      height = 1,
      r,
      x = d3_hexbinX,
      y = d3_hexbinY,
      dx,
      dy;

  function hexbin(points) {
    var binsById = {};

    points.forEach(function(point, i) {
      var py = y.call(hexbin, point, i) / dy, pj = Math.round(py),
          px = x.call(hexbin, point, i) / dx - (pj & 1 ? .5 : 0), pi = Math.round(px),
          py1 = py - pj;

      if (Math.abs(py1) * 3 > 1) {
        var px1 = px - pi,
            pi2 = pi + (px < pi ? -1 : 1) / 2,
            pj2 = pj + (py < pj ? -1 : 1),
            px2 = px - pi2,
            py2 = py - pj2;
        if (px1 * px1 + py1 * py1 > px2 * px2 + py2 * py2) pi = pi2 + (pj & 1 ? 1 : -1) / 2, pj = pj2;
      }

      var id = pi + "-" + pj, bin = binsById[id];
      if (bin) bin.push(point); else {
        bin = binsById[id] = [point];
        bin.i = pi;
        bin.j = pj;
        bin.x = (pi + (pj & 1 ? 1 / 2 : 0)) * dx;
        bin.y = pj * dy;
      }
    });

    return d3.values(binsById);
  }

  function hexagon(radius) {
    var x0 = 0, y0 = 0;
    return d3_hexbinAngles.map(function(angle) {
      var x1 = Math.sin(angle) * radius,
          y1 = -Math.cos(angle) * radius,
          dx = x1 - x0,
          dy = y1 - y0;
      x0 = x1, y0 = y1;
      return [dx, dy];
    });
  }

  hexbin.x = function(_) {
    if (!arguments.length) return x;
    x = _;
    return hexbin;
  };

  hexbin.y = function(_) {
    if (!arguments.length) return y;
    y = _;
    return hexbin;
  };

  hexbin.hexagon = function(radius) {
    if (arguments.length < 1) radius = r;
    return "m" + hexagon(radius).join("l") + "z";
  };

  hexbin.centers = function() {
    var centers = [];
    for (var y = 0, odd = false, j = 0; y < height + r; y += dy, odd = !odd, ++j) {
      for (var x = odd ? dx / 2 : 0, i = 0; x < width + dx / 2; x += dx, ++i) {
        var center = [x, y];
        center.i = i;
        center.j = j;
        centers.push(center);
      }
    }
    return centers;
  };

  hexbin.mesh = function() {
    var fragment = hexagon(r).slice(0, 4).join("l");
    return hexbin.centers().map(function(p) { return "M" + p + "m" + fragment; }).join("");
  };

  hexbin.size = function(_) {
    if (!arguments.length) return [width, height];
    width = +_[0], height = +_[1];
    return hexbin;
  };

  hexbin.radius = function(_) {
    if (!arguments.length) return r;
    r = +_;
    dx = r * 2 * Math.sin(Math.PI / 3);
    dy = r * 1.5;
    return hexbin;
  };

  return hexbin.radius(1);
};

var d3_hexbinAngles = d3.range(0, 2 * Math.PI, Math.PI / 3),
    d3_hexbinX = function(d) { return d[0]; },
    d3_hexbinY = function(d) { return d[1]; };


////////////////////////////////////////////////////////////////////////////////
//// Definition of lodash (_), used by square plots
////////////////////////////////////////////////////////////////////////////////
/**
 * @license
 * lodash 3.10.1 (Custom Build) lodash.com/license | Underscore.js 1.8.3 underscorejs.org/LICENSE
 * Build: `lodash modern -o ./lodash.js`
 */
;(function(){function n(n,t){if(n!==t){var r=null===n,e=n===w,u=n===n,o=null===t,i=t===w,f=t===t;if(n>t&&!o||!u||r&&!i&&f||e&&f)return 1;if(n<t&&!r||!f||o&&!e&&u||i&&u)return-1}return 0}function t(n,t,r){for(var e=n.length,u=r?e:-1;r?u--:++u<e;)if(t(n[u],u,n))return u;return-1}function r(n,t,r){if(t!==t)return p(n,r);r-=1;for(var e=n.length;++r<e;)if(n[r]===t)return r;return-1}function e(n){return typeof n=="function"||false}function u(n){return null==n?"":n+""}function o(n,t){for(var r=-1,e=n.length;++r<e&&-1<t.indexOf(n.charAt(r)););return r}function i(n,t){for(var r=n.length;r--&&-1<t.indexOf(n.charAt(r)););return r}function f(t,r){return n(t.a,r.a)||t.b-r.b}function a(n){return Nn[n]}function c(n){return Tn[n]}function l(n,t,r){return t?n=Bn[n]:r&&(n=Dn[n]),"\\"+n}function s(n){return"\\"+Dn[n]}function p(n,t,r){var e=n.length;for(t+=r?0:-1;r?t--:++t<e;){var u=n[t];if(u!==u)return t}return-1}function h(n){return!!n&&typeof n=="object"}function _(n){return 160>=n&&9<=n&&13>=n||32==n||160==n||5760==n||6158==n||8192<=n&&(8202>=n||8232==n||8233==n||8239==n||8287==n||12288==n||65279==n);}function v(n,t){for(var r=-1,e=n.length,u=-1,o=[];++r<e;)n[r]===t&&(n[r]=z,o[++u]=r);return o}function g(n){for(var t=-1,r=n.length;++t<r&&_(n.charCodeAt(t)););return t}function y(n){for(var t=n.length;t--&&_(n.charCodeAt(t)););return t}function d(n){return Ln[n]}function m(_){function Nn(n){if(h(n)&&!(Oo(n)||n instanceof zn)){if(n instanceof Ln)return n;if(nu.call(n,"__chain__")&&nu.call(n,"__wrapped__"))return Mr(n)}return new Ln(n)}function Tn(){}function Ln(n,t,r){this.__wrapped__=n,this.__actions__=r||[],this.__chain__=!!t}function zn(n){this.__wrapped__=n,this.__actions__=[],this.__dir__=1,this.__filtered__=false,this.__iteratees__=[],this.__takeCount__=Ru,this.__views__=[]}function Bn(){this.__data__={}}function Dn(n){var t=n?n.length:0;for(this.data={hash:gu(null),set:new lu};t--;)this.push(n[t])}function Mn(n,t){var r=n.data;return(typeof t=="string"||ge(t)?r.set.has(t):r.hash[t])?0:-1}function qn(n,t){var r=-1,e=n.length;for(t||(t=Be(e));++r<e;)t[r]=n[r];return t}function Pn(n,t){for(var r=-1,e=n.length;++r<e&&false!==t(n[r],r,n););return n}function Kn(n,t){for(var r=-1,e=n.length;++r<e;)if(!t(n[r],r,n))return false;return true}function Vn(n,t){for(var r=-1,e=n.length,u=-1,o=[];++r<e;){var i=n[r];t(i,r,n)&&(o[++u]=i)}return o}function Gn(n,t){for(var r=-1,e=n.length,u=Be(e);++r<e;)u[r]=t(n[r],r,n);return u}function Jn(n,t){for(var r=-1,e=t.length,u=n.length;++r<e;)n[u+r]=t[r];return n}function Xn(n,t,r,e){var u=-1,o=n.length;for(e&&o&&(r=n[++u]);++u<o;)r=t(r,n[u],u,n);return r}function Hn(n,t){for(var r=-1,e=n.length;++r<e;)if(t(n[r],r,n))return true;return false}function Qn(n,t,r,e){return n!==w&&nu.call(e,r)?n:t}function nt(n,t,r){for(var e=-1,u=zo(t),o=u.length;++e<o;){var i=u[e],f=n[i],a=r(f,t[i],i,n,t);(a===a?a===f:f!==f)&&(f!==w||i in n)||(n[i]=a)}return n}function tt(n,t){return null==t?n:et(t,zo(t),n)}function rt(n,t){for(var r=-1,e=null==n,u=!e&&Er(n),o=u?n.length:0,i=t.length,f=Be(i);++r<i;){var a=t[r];f[r]=u?Cr(a,o)?n[a]:w:e?w:n[a]}return f}function et(n,t,r){r||(r={});for(var e=-1,u=t.length;++e<u;){var o=t[e];r[o]=n[o]}return r}function ut(n,t,r){var e=typeof n;return"function"==e?t===w?n:Bt(n,t,r):null==n?Fe:"object"==e?bt(n):t===w?ze(n):xt(n,t)}function ot(n,t,r,e,u,o,i){var f;if(r&&(f=u?r(n,e,u):r(n)),f!==w)return f;if(!ge(n))return n;if(e=Oo(n)){if(f=kr(n),!t)return qn(n,f)}else{var a=ru.call(n),c=a==K;if(a!=Z&&a!=B&&(!c||u))return Fn[a]?Rr(n,a,t):u?n:{};if(f=Ir(c?{}:n),!t)return tt(f,n)}for(o||(o=[]),i||(i=[]),u=o.length;u--;)if(o[u]==n)return i[u];return o.push(n),i.push(f),(e?Pn:_t)(n,function(e,u){f[u]=ot(e,t,r,u,n,o,i)}),f}function it(n,t,r){if(typeof n!="function")throw new Ge(L);return su(function(){n.apply(w,r)},t)}function ft(n,t){var e=n?n.length:0,u=[];if(!e)return u;var o=-1,i=xr(),f=i===r,a=f&&t.length>=F&&gu&&lu?new Dn(t):null,c=t.length;a&&(i=Mn,f=false,t=a);n:for(;++o<e;)if(a=n[o],f&&a===a){for(var l=c;l--;)if(t[l]===a)continue n;u.push(a)}else 0>i(t,a,0)&&u.push(a);return u}function at(n,t){var r=true;return Su(n,function(n,e,u){return r=!!t(n,e,u)}),r}function ct(n,t,r,e){var u=e,o=u;return Su(n,function(n,i,f){i=+t(n,i,f),(r(i,u)||i===e&&i===o)&&(u=i,o=n)}),o}function lt(n,t){var r=[];return Su(n,function(n,e,u){t(n,e,u)&&r.push(n)}),r}function st(n,t,r,e){var u;return r(n,function(n,r,o){return t(n,r,o)?(u=e?r:n,false):void 0}),u}function pt(n,t,r,e){e||(e=[]);for(var u=-1,o=n.length;++u<o;){var i=n[u];h(i)&&Er(i)&&(r||Oo(i)||pe(i))?t?pt(i,t,r,e):Jn(e,i):r||(e[e.length]=i)}return e}function ht(n,t){Nu(n,t,Re)}function _t(n,t){return Nu(n,t,zo)}function vt(n,t){return Tu(n,t,zo)}function gt(n,t){for(var r=-1,e=t.length,u=-1,o=[];++r<e;){var i=t[r];ve(n[i])&&(o[++u]=i)}return o}function yt(n,t,r){if(null!=n){r!==w&&r in Br(n)&&(t=[r]),r=0;for(var e=t.length;null!=n&&r<e;)n=n[t[r++]];return r&&r==e?n:w}}function dt(n,t,r,e,u,o){if(n===t)n=true;else if(null==n||null==t||!ge(n)&&!h(t))n=n!==n&&t!==t;else n:{var i=dt,f=Oo(n),a=Oo(t),c=D,l=D;f||(c=ru.call(n),c==B?c=Z:c!=Z&&(f=xe(n))),a||(l=ru.call(t),l==B?l=Z:l!=Z&&xe(t));var s=c==Z,a=l==Z,l=c==l;if(!l||f||s){if(!e&&(c=s&&nu.call(n,"__wrapped__"),a=a&&nu.call(t,"__wrapped__"),c||a)){n=i(c?n.value():n,a?t.value():t,r,e,u,o);break n}if(l){for(u||(u=[]),o||(o=[]),c=u.length;c--;)if(u[c]==n){n=o[c]==t;break n}u.push(n),o.push(t),n=(f?yr:mr)(n,t,i,r,e,u,o),u.pop(),o.pop()}else n=false}else n=dr(n,t,c)}return n}function mt(n,t,r){var e=t.length,u=e,o=!r;if(null==n)return!u;for(n=Br(n);e--;){var i=t[e];if(o&&i[2]?i[1]!==n[i[0]]:!(i[0]in n))return false}for(;++e<u;){var i=t[e],f=i[0],a=n[f],c=i[1];if(o&&i[2]){if(a===w&&!(f in n))return false}else if(i=r?r(a,c,f):w,i===w?!dt(c,a,r,true):!i)return false}return true}function wt(n,t){var r=-1,e=Er(n)?Be(n.length):[];return Su(n,function(n,u,o){e[++r]=t(n,u,o)}),e}function bt(n){var t=Ar(n);if(1==t.length&&t[0][2]){var r=t[0][0],e=t[0][1];return function(n){return null==n?false:n[r]===e&&(e!==w||r in Br(n))}}return function(n){return mt(n,t)}}function xt(n,t){var r=Oo(n),e=Wr(n)&&t===t&&!ge(t),u=n+"";return n=Dr(n),function(o){if(null==o)return false;var i=u;if(o=Br(o),!(!r&&e||i in o)){if(o=1==n.length?o:yt(o,Et(n,0,-1)),null==o)return false;i=Zr(n),o=Br(o)}return o[i]===t?t!==w||i in o:dt(t,o[i],w,true)}}function At(n,t,r,e,u){if(!ge(n))return n;var o=Er(t)&&(Oo(t)||xe(t)),i=o?w:zo(t);return Pn(i||t,function(f,a){if(i&&(a=f,f=t[a]),h(f)){e||(e=[]),u||(u=[]);n:{for(var c=a,l=e,s=u,p=l.length,_=t[c];p--;)if(l[p]==_){n[c]=s[p];break n}var p=n[c],v=r?r(p,_,c,n,t):w,g=v===w;g&&(v=_,Er(_)&&(Oo(_)||xe(_))?v=Oo(p)?p:Er(p)?qn(p):[]:me(_)||pe(_)?v=pe(p)?ke(p):me(p)?p:{}:g=false),l.push(_),s.push(v),g?n[c]=At(v,_,r,l,s):(v===v?v!==p:p===p)&&(n[c]=v)}}else c=n[a],l=r?r(c,f,a,n,t):w,(s=l===w)&&(l=f),l===w&&(!o||a in n)||!s&&(l===l?l===c:c!==c)||(n[a]=l);}),n}function jt(n){return function(t){return null==t?w:t[n]}}function kt(n){var t=n+"";return n=Dr(n),function(r){return yt(r,n,t)}}function It(n,t){for(var r=n?t.length:0;r--;){var e=t[r];if(e!=u&&Cr(e)){var u=e;pu.call(n,e,1)}}}function Rt(n,t){return n+yu(ku()*(t-n+1))}function Ot(n,t,r,e,u){return u(n,function(n,u,o){r=e?(e=false,n):t(r,n,u,o)}),r}function Et(n,t,r){var e=-1,u=n.length;for(t=null==t?0:+t||0,0>t&&(t=-t>u?0:u+t),r=r===w||r>u?u:+r||0,0>r&&(r+=u),u=t>r?0:r-t>>>0,t>>>=0,r=Be(u);++e<u;)r[e]=n[e+t];return r}function Ct(n,t){var r;return Su(n,function(n,e,u){return r=t(n,e,u),!r}),!!r}function Ut(n,t){var r=n.length;for(n.sort(t);r--;)n[r]=n[r].c;return n}function Wt(t,r,e){var u=wr(),o=-1;return r=Gn(r,function(n){return u(n)}),t=wt(t,function(n){return{a:Gn(r,function(t){return t(n)}),b:++o,c:n}}),Ut(t,function(t,r){var u;n:{for(var o=-1,i=t.a,f=r.a,a=i.length,c=e.length;++o<a;)if(u=n(i[o],f[o])){if(o>=c)break n;o=e[o],u*="asc"===o||true===o?1:-1;break n}u=t.b-r.b}return u})}function $t(n,t){var r=0;return Su(n,function(n,e,u){r+=+t(n,e,u)||0}),r}function St(n,t){var e=-1,u=xr(),o=n.length,i=u===r,f=i&&o>=F,a=f&&gu&&lu?new Dn(void 0):null,c=[];a?(u=Mn,i=false):(f=false,a=t?[]:c);n:for(;++e<o;){var l=n[e],s=t?t(l,e,n):l;if(i&&l===l){for(var p=a.length;p--;)if(a[p]===s)continue n;t&&a.push(s),c.push(l)}else 0>u(a,s,0)&&((t||f)&&a.push(s),c.push(l))}return c}function Ft(n,t){for(var r=-1,e=t.length,u=Be(e);++r<e;)u[r]=n[t[r]];return u}function Nt(n,t,r,e){for(var u=n.length,o=e?u:-1;(e?o--:++o<u)&&t(n[o],o,n););return r?Et(n,e?0:o,e?o+1:u):Et(n,e?o+1:0,e?u:o)}function Tt(n,t){var r=n;r instanceof zn&&(r=r.value());for(var e=-1,u=t.length;++e<u;)var o=t[e],r=o.func.apply(o.thisArg,Jn([r],o.args));return r}function Lt(n,t,r){var e=0,u=n?n.length:e;if(typeof t=="number"&&t===t&&u<=Eu){for(;e<u;){var o=e+u>>>1,i=n[o];(r?i<=t:i<t)&&null!==i?e=o+1:u=o}return u}return zt(n,t,Fe,r)}function zt(n,t,r,e){t=r(t);for(var u=0,o=n?n.length:0,i=t!==t,f=null===t,a=t===w;u<o;){var c=yu((u+o)/2),l=r(n[c]),s=l!==w,p=l===l;(i?p||e:f?p&&s&&(e||null!=l):a?p&&(e||s):null==l?0:e?l<=t:l<t)?u=c+1:o=c}return xu(o,Ou)}function Bt(n,t,r){if(typeof n!="function")return Fe;if(t===w)return n;switch(r){case 1:return function(r){return n.call(t,r)};case 3:return function(r,e,u){return n.call(t,r,e,u)};case 4:return function(r,e,u,o){return n.call(t,r,e,u,o)};case 5:return function(r,e,u,o,i){return n.call(t,r,e,u,o,i)}}return function(){return n.apply(t,arguments)}}function Dt(n){var t=new ou(n.byteLength);return new hu(t).set(new hu(n)),t}function Mt(n,t,r){for(var e=r.length,u=-1,o=bu(n.length-e,0),i=-1,f=t.length,a=Be(f+o);++i<f;)a[i]=t[i];for(;++u<e;)a[r[u]]=n[u];for(;o--;)a[i++]=n[u++];return a}function qt(n,t,r){for(var e=-1,u=r.length,o=-1,i=bu(n.length-u,0),f=-1,a=t.length,c=Be(i+a);++o<i;)c[o]=n[o];for(i=o;++f<a;)c[i+f]=t[f];for(;++e<u;)c[i+r[e]]=n[o++];return c}function Pt(n,t){return function(r,e,u){var o=t?t():{};if(e=wr(e,u,3),Oo(r)){u=-1;for(var i=r.length;++u<i;){var f=r[u];n(o,f,e(f,u,r),r)}}else Su(r,function(t,r,u){n(o,t,e(t,r,u),u)});return o}}function Kt(n){return le(function(t,r){var e=-1,u=null==t?0:r.length,o=2<u?r[u-2]:w,i=2<u?r[2]:w,f=1<u?r[u-1]:w;for(typeof o=="function"?(o=Bt(o,f,5),u-=2):(o=typeof f=="function"?f:w,u-=o?1:0),i&&Ur(r[0],r[1],i)&&(o=3>u?w:o,u=1);++e<u;)(i=r[e])&&n(t,i,o);return t})}function Vt(n,t){return function(r,e){var u=r?Bu(r):0;if(!Sr(u))return n(r,e);for(var o=t?u:-1,i=Br(r);(t?o--:++o<u)&&false!==e(i[o],o,i););return r}}function Zt(n){return function(t,r,e){var u=Br(t);e=e(t);for(var o=e.length,i=n?o:-1;n?i--:++i<o;){var f=e[i];if(false===r(u[f],f,u))break}return t}}function Yt(n,t){function r(){return(this&&this!==Zn&&this instanceof r?e:n).apply(t,arguments)}var e=Jt(n);return r}function Gt(n){return function(t){var r=-1;t=$e(Ce(t));for(var e=t.length,u="";++r<e;)u=n(u,t[r],r);return u}}function Jt(n){return function(){var t=arguments;switch(t.length){case 0:return new n;case 1:return new n(t[0]);case 2:return new n(t[0],t[1]);case 3:return new n(t[0],t[1],t[2]);case 4:return new n(t[0],t[1],t[2],t[3]);case 5:return new n(t[0],t[1],t[2],t[3],t[4]);case 6:return new n(t[0],t[1],t[2],t[3],t[4],t[5]);case 7:return new n(t[0],t[1],t[2],t[3],t[4],t[5],t[6])}var r=$u(n.prototype),t=n.apply(r,t);return ge(t)?t:r}}function Xt(n){function t(r,e,u){return u&&Ur(r,e,u)&&(e=w),r=gr(r,n,w,w,w,w,w,e),r.placeholder=t.placeholder,r}return t}function Ht(n,t){return le(function(r){var e=r[0];return null==e?e:(r.push(t),n.apply(w,r))})}function Qt(n,t){return function(r,e,u){if(u&&Ur(r,e,u)&&(e=w),e=wr(e,u,3),1==e.length){u=r=Oo(r)?r:zr(r);for(var o=e,i=-1,f=u.length,a=t,c=a;++i<f;){var l=u[i],s=+o(l);n(s,a)&&(a=s,c=l)}if(u=c,!r.length||u!==t)return u}return ct(r,e,n,t)}}function nr(n,r){return function(e,u,o){return u=wr(u,o,3),Oo(e)?(u=t(e,u,r),-1<u?e[u]:w):st(e,u,n)}}function tr(n){return function(r,e,u){return r&&r.length?(e=wr(e,u,3),t(r,e,n)):-1}}function rr(n){return function(t,r,e){return r=wr(r,e,3),st(t,r,n,true)}}function er(n){return function(){for(var t,r=arguments.length,e=n?r:-1,u=0,o=Be(r);n?e--:++e<r;){var i=o[u++]=arguments[e];if(typeof i!="function")throw new Ge(L);!t&&Ln.prototype.thru&&"wrapper"==br(i)&&(t=new Ln([],true))}for(e=t?-1:r;++e<r;){var i=o[e],u=br(i),f="wrapper"==u?zu(i):w;t=f&&$r(f[0])&&f[1]==(E|k|R|C)&&!f[4].length&&1==f[9]?t[br(f[0])].apply(t,f[3]):1==i.length&&$r(i)?t[u]():t.thru(i)}return function(){var n=arguments,e=n[0];if(t&&1==n.length&&Oo(e)&&e.length>=F)return t.plant(e).value();for(var u=0,n=r?o[u].apply(this,n):e;++u<r;)n=o[u].call(this,n);return n}}}function ur(n,t){return function(r,e,u){return typeof e=="function"&&u===w&&Oo(r)?n(r,e):t(r,Bt(e,u,3))}}function or(n){return function(t,r,e){return(typeof r!="function"||e!==w)&&(r=Bt(r,e,3)),n(t,r,Re)}}function ir(n){return function(t,r,e){return(typeof r!="function"||e!==w)&&(r=Bt(r,e,3)),n(t,r)}}function fr(n){return function(t,r,e){var u={};return r=wr(r,e,3),_t(t,function(t,e,o){o=r(t,e,o),e=n?o:e,t=n?t:o,u[e]=t}),u}}function ar(n){return function(t,r,e){return t=u(t),(n?t:"")+pr(t,r,e)+(n?"":t)}}function cr(n){var t=le(function(r,e){var u=v(e,t.placeholder);return gr(r,n,w,e,u)});return t}function lr(n,t){return function(r,e,u,o){var i=3>arguments.length;return typeof e=="function"&&o===w&&Oo(r)?n(r,e,u,i):Ot(r,wr(e,o,4),u,i,t)}}function sr(n,t,r,e,u,o,i,f,a,c){function l(){for(var m=arguments.length,b=m,j=Be(m);b--;)j[b]=arguments[b];if(e&&(j=Mt(j,e,u)),o&&(j=qt(j,o,i)),_||y){var b=l.placeholder,k=v(j,b),m=m-k.length;if(m<c){var I=f?qn(f):w,m=bu(c-m,0),E=_?k:w,k=_?w:k,C=_?j:w,j=_?w:j;return t|=_?R:O,t&=~(_?O:R),g||(t&=~(x|A)),j=[n,t,r,C,E,j,k,I,a,m],I=sr.apply(w,j),$r(n)&&Du(I,j),I.placeholder=b,I}}if(b=p?r:this,I=h?b[n]:n,f)for(m=j.length,E=xu(f.length,m),k=qn(j);E--;)C=f[E],j[E]=Cr(C,m)?k[C]:w;return s&&a<j.length&&(j.length=a),this&&this!==Zn&&this instanceof l&&(I=d||Jt(n)),I.apply(b,j)}var s=t&E,p=t&x,h=t&A,_=t&k,g=t&j,y=t&I,d=h?w:Jt(n);return l}function pr(n,t,r){return n=n.length,t=+t,n<t&&mu(t)?(t-=n,r=null==r?" ":r+"",Ue(r,vu(t/r.length)).slice(0,t)):""}function hr(n,t,r,e){function u(){for(var t=-1,f=arguments.length,a=-1,c=e.length,l=Be(c+f);++a<c;)l[a]=e[a];for(;f--;)l[a++]=arguments[++t];return(this&&this!==Zn&&this instanceof u?i:n).apply(o?r:this,l)}var o=t&x,i=Jt(n);return u}function _r(n){var t=Pe[n];return function(n,r){return(r=r===w?0:+r||0)?(r=au(10,r),t(n*r)/r):t(n)}}function vr(n){return function(t,r,e,u){var o=wr(e);return null==e&&o===ut?Lt(t,r,n):zt(t,r,o(e,u,1),n)}}function gr(n,t,r,e,u,o,i,f){var a=t&A;if(!a&&typeof n!="function")throw new Ge(L);var c=e?e.length:0;if(c||(t&=~(R|O),e=u=w),c-=u?u.length:0,t&O){var l=e,s=u;e=u=w}var p=a?w:zu(n);return r=[n,t,r,e,u,l,s,o,i,f],p&&(e=r[1],t=p[1],f=e|t,u=t==E&&e==k||t==E&&e==C&&r[7].length<=p[8]||t==(E|C)&&e==k,(f<E||u)&&(t&x&&(r[2]=p[2],f|=e&x?0:j),(e=p[3])&&(u=r[3],r[3]=u?Mt(u,e,p[4]):qn(e),r[4]=u?v(r[3],z):qn(p[4])),(e=p[5])&&(u=r[5],r[5]=u?qt(u,e,p[6]):qn(e),r[6]=u?v(r[5],z):qn(p[6])),(e=p[7])&&(r[7]=qn(e)),t&E&&(r[8]=null==r[8]?p[8]:xu(r[8],p[8])),null==r[9]&&(r[9]=p[9]),r[0]=p[0],r[1]=f),t=r[1],f=r[9]),r[9]=null==f?a?0:n.length:bu(f-c,0)||0,(p?Lu:Du)(t==x?Yt(r[0],r[2]):t!=R&&t!=(x|R)||r[4].length?sr.apply(w,r):hr.apply(w,r),r);}function yr(n,t,r,e,u,o,i){var f=-1,a=n.length,c=t.length;if(a!=c&&(!u||c<=a))return false;for(;++f<a;){var l=n[f],c=t[f],s=e?e(u?c:l,u?l:c,f):w;if(s!==w){if(s)continue;return false}if(u){if(!Hn(t,function(n){return l===n||r(l,n,e,u,o,i)}))return false}else if(l!==c&&!r(l,c,e,u,o,i))return false}return true}function dr(n,t,r){switch(r){case M:case q:return+n==+t;case P:return n.name==t.name&&n.message==t.message;case V:return n!=+n?t!=+t:n==+t;case Y:case G:return n==t+""}return false}function mr(n,t,r,e,u,o,i){var f=zo(n),a=f.length,c=zo(t).length;if(a!=c&&!u)return false;for(c=a;c--;){var l=f[c];if(!(u?l in t:nu.call(t,l)))return false}for(var s=u;++c<a;){var l=f[c],p=n[l],h=t[l],_=e?e(u?h:p,u?p:h,l):w;if(_===w?!r(p,h,e,u,o,i):!_)return false;s||(s="constructor"==l)}return s||(r=n.constructor,e=t.constructor,!(r!=e&&"constructor"in n&&"constructor"in t)||typeof r=="function"&&r instanceof r&&typeof e=="function"&&e instanceof e)?true:false}function wr(n,t,r){var e=Nn.callback||Se,e=e===Se?ut:e;return r?e(n,t,r):e}function br(n){for(var t=n.name+"",r=Wu[t],e=r?r.length:0;e--;){var u=r[e],o=u.func;if(null==o||o==n)return u.name}return t}function xr(n,t,e){var u=Nn.indexOf||Vr,u=u===Vr?r:u;return n?u(n,t,e):u}function Ar(n){n=Oe(n);for(var t=n.length;t--;){var r=n[t][1];n[t][2]=r===r&&!ge(r)}return n}function jr(n,t){var r=null==n?w:n[t];return ye(r)?r:w}function kr(n){var t=n.length,r=new n.constructor(t);return t&&"string"==typeof n[0]&&nu.call(n,"index")&&(r.index=n.index,r.input=n.input),r}function Ir(n){return n=n.constructor,typeof n=="function"&&n instanceof n||(n=Ve),new n}function Rr(n,t,r){var e=n.constructor;switch(t){case J:return Dt(n);case M:case q:return new e(+n);case X:case H:case Q:case nn:case tn:case rn:case en:case un:case on:return t=n.buffer,new e(r?Dt(t):t,n.byteOffset,n.length);case V:case G:return new e(n);case Y:var u=new e(n.source,kn.exec(n));u.lastIndex=n.lastIndex}return u}function Or(n,t,r){return null==n||Wr(t,n)||(t=Dr(t),n=1==t.length?n:yt(n,Et(t,0,-1)),t=Zr(t)),t=null==n?n:n[t],null==t?w:t.apply(n,r)}function Er(n){return null!=n&&Sr(Bu(n));}function Cr(n,t){return n=typeof n=="number"||On.test(n)?+n:-1,t=null==t?Cu:t,-1<n&&0==n%1&&n<t}function Ur(n,t,r){if(!ge(r))return false;var e=typeof t;return("number"==e?Er(r)&&Cr(t,r.length):"string"==e&&t in r)?(t=r[t],n===n?n===t:t!==t):false}function Wr(n,t){var r=typeof n;return"string"==r&&dn.test(n)||"number"==r?true:Oo(n)?false:!yn.test(n)||null!=t&&n in Br(t)}function $r(n){var t=br(n),r=Nn[t];return typeof r=="function"&&t in zn.prototype?n===r?true:(t=zu(r),!!t&&n===t[0]):false}function Sr(n){return typeof n=="number"&&-1<n&&0==n%1&&n<=Cu;}function Fr(n,t){return n===w?t:Eo(n,t,Fr)}function Nr(n,t){n=Br(n);for(var r=-1,e=t.length,u={};++r<e;){var o=t[r];o in n&&(u[o]=n[o])}return u}function Tr(n,t){var r={};return ht(n,function(n,e,u){t(n,e,u)&&(r[e]=n)}),r}function Lr(n){for(var t=Re(n),r=t.length,e=r&&n.length,u=!!e&&Sr(e)&&(Oo(n)||pe(n)),o=-1,i=[];++o<r;){var f=t[o];(u&&Cr(f,e)||nu.call(n,f))&&i.push(f)}return i}function zr(n){return null==n?[]:Er(n)?ge(n)?n:Ve(n):Ee(n)}function Br(n){return ge(n)?n:Ve(n)}function Dr(n){if(Oo(n))return n;var t=[];return u(n).replace(mn,function(n,r,e,u){t.push(e?u.replace(An,"$1"):r||n)}),t}function Mr(n){return n instanceof zn?n.clone():new Ln(n.__wrapped__,n.__chain__,qn(n.__actions__))}function qr(n,t,r){return n&&n.length?((r?Ur(n,t,r):null==t)&&(t=1),Et(n,0>t?0:t)):[]}function Pr(n,t,r){var e=n?n.length:0;return e?((r?Ur(n,t,r):null==t)&&(t=1),t=e-(+t||0),Et(n,0,0>t?0:t)):[]}function Kr(n){return n?n[0]:w}function Vr(n,t,e){var u=n?n.length:0;if(!u)return-1;if(typeof e=="number")e=0>e?bu(u+e,0):e;else if(e)return e=Lt(n,t),e<u&&(t===t?t===n[e]:n[e]!==n[e])?e:-1;return r(n,t,e||0)}function Zr(n){var t=n?n.length:0;return t?n[t-1]:w}function Yr(n){return qr(n,1)}function Gr(n,t,e,u){if(!n||!n.length)return[];null!=t&&typeof t!="boolean"&&(u=e,e=Ur(n,t,u)?w:t,t=false);var o=wr();if((null!=e||o!==ut)&&(e=o(e,u,3)),t&&xr()===r){t=e;var i;e=-1,u=n.length;for(var o=-1,f=[];++e<u;){var a=n[e],c=t?t(a,e,n):a;e&&i===c||(i=c,f[++o]=a)}n=f}else n=St(n,e);return n}function Jr(n){if(!n||!n.length)return[];var t=-1,r=0;n=Vn(n,function(n){return Er(n)?(r=bu(n.length,r),true):void 0});for(var e=Be(r);++t<r;)e[t]=Gn(n,jt(t));return e}function Xr(n,t,r){return n&&n.length?(n=Jr(n),null==t?n:(t=Bt(t,r,4),Gn(n,function(n){return Xn(n,t,w,true)}))):[]}function Hr(n,t){var r=-1,e=n?n.length:0,u={};for(!e||t||Oo(n[0])||(t=[]);++r<e;){var o=n[r];t?u[o]=t[r]:o&&(u[o[0]]=o[1])}return u}function Qr(n){return n=Nn(n),n.__chain__=true,n}function ne(n,t,r){return t.call(r,n)}function te(n,t,r){var e=Oo(n)?Kn:at;return r&&Ur(n,t,r)&&(t=w),(typeof t!="function"||r!==w)&&(t=wr(t,r,3)),e(n,t)}function re(n,t,r){var e=Oo(n)?Vn:lt;return t=wr(t,r,3),e(n,t)}function ee(n,t,r,e){var u=n?Bu(n):0;return Sr(u)||(n=Ee(n),u=n.length),r=typeof r!="number"||e&&Ur(t,r,e)?0:0>r?bu(u+r,0):r||0,typeof n=="string"||!Oo(n)&&be(n)?r<=u&&-1<n.indexOf(t,r):!!u&&-1<xr(n,t,r)}function ue(n,t,r){var e=Oo(n)?Gn:wt;return t=wr(t,r,3),e(n,t)}function oe(n,t,r){if(r?Ur(n,t,r):null==t){n=zr(n);var e=n.length;return 0<e?n[Rt(0,e-1)]:w}r=-1,n=je(n);var e=n.length,u=e-1;for(t=xu(0>t?0:+t||0,e);++r<t;){var e=Rt(r,u),o=n[e];n[e]=n[r],n[r]=o}return n.length=t,n}function ie(n,t,r){var e=Oo(n)?Hn:Ct;return r&&Ur(n,t,r)&&(t=w),(typeof t!="function"||r!==w)&&(t=wr(t,r,3)),e(n,t)}function fe(n,t){var r;if(typeof t!="function"){if(typeof n!="function")throw new Ge(L);var e=n;n=t,t=e}return function(){return 0<--n&&(r=t.apply(this,arguments)),1>=n&&(t=w),r}}function ae(n,t,r){function e(t,r){r&&iu(r),a=p=h=w,t&&(_=ho(),c=n.apply(s,f),p||a||(f=s=w))}function u(){var n=t-(ho()-l);0>=n||n>t?e(h,a):p=su(u,n)}function o(){e(g,p);}function i(){if(f=arguments,l=ho(),s=this,h=g&&(p||!y),false===v)var r=y&&!p;else{a||y||(_=l);var e=v-(l-_),i=0>=e||e>v;i?(a&&(a=iu(a)),_=l,c=n.apply(s,f)):a||(a=su(o,e))}return i&&p?p=iu(p):p||t===v||(p=su(u,t)),r&&(i=true,c=n.apply(s,f)),!i||p||a||(f=s=w),c}var f,a,c,l,s,p,h,_=0,v=false,g=true;if(typeof n!="function")throw new Ge(L);if(t=0>t?0:+t||0,true===r)var y=true,g=false;else ge(r)&&(y=!!r.leading,v="maxWait"in r&&bu(+r.maxWait||0,t),g="trailing"in r?!!r.trailing:g);return i.cancel=function(){p&&iu(p),a&&iu(a),_=0,a=p=h=w},i}function ce(n,t){function r(){var e=arguments,u=t?t.apply(this,e):e[0],o=r.cache;return o.has(u)?o.get(u):(e=n.apply(this,e),r.cache=o.set(u,e),e)}if(typeof n!="function"||t&&typeof t!="function")throw new Ge(L);return r.cache=new ce.Cache,r}function le(n,t){if(typeof n!="function")throw new Ge(L);return t=bu(t===w?n.length-1:+t||0,0),function(){for(var r=arguments,e=-1,u=bu(r.length-t,0),o=Be(u);++e<u;)o[e]=r[t+e];switch(t){case 0:return n.call(this,o);case 1:return n.call(this,r[0],o);case 2:return n.call(this,r[0],r[1],o)}for(u=Be(t+1),e=-1;++e<t;)u[e]=r[e];return u[t]=o,n.apply(this,u)}}function se(n,t){return n>t}function pe(n){return h(n)&&Er(n)&&nu.call(n,"callee")&&!cu.call(n,"callee")}function he(n,t,r,e){return e=(r=typeof r=="function"?Bt(r,e,3):w)?r(n,t):w,e===w?dt(n,t,r):!!e}function _e(n){return h(n)&&typeof n.message=="string"&&ru.call(n)==P}function ve(n){return ge(n)&&ru.call(n)==K}function ge(n){var t=typeof n;return!!n&&("object"==t||"function"==t)}function ye(n){return null==n?false:ve(n)?uu.test(Qe.call(n)):h(n)&&Rn.test(n)}function de(n){return typeof n=="number"||h(n)&&ru.call(n)==V}function me(n){var t;if(!h(n)||ru.call(n)!=Z||pe(n)||!(nu.call(n,"constructor")||(t=n.constructor,typeof t!="function"||t instanceof t)))return false;var r;return ht(n,function(n,t){r=t}),r===w||nu.call(n,r)}function we(n){return ge(n)&&ru.call(n)==Y}function be(n){return typeof n=="string"||h(n)&&ru.call(n)==G}function xe(n){return h(n)&&Sr(n.length)&&!!Sn[ru.call(n)]}function Ae(n,t){return n<t}function je(n){var t=n?Bu(n):0;return Sr(t)?t?qn(n):[]:Ee(n)}function ke(n){return et(n,Re(n))}function Ie(n){return gt(n,Re(n))}function Re(n){if(null==n)return[];ge(n)||(n=Ve(n));for(var t=n.length,t=t&&Sr(t)&&(Oo(n)||pe(n))&&t||0,r=n.constructor,e=-1,r=typeof r=="function"&&r.prototype===n,u=Be(t),o=0<t;++e<t;)u[e]=e+"";for(var i in n)o&&Cr(i,t)||"constructor"==i&&(r||!nu.call(n,i))||u.push(i);return u}function Oe(n){n=Br(n);for(var t=-1,r=zo(n),e=r.length,u=Be(e);++t<e;){var o=r[t];u[t]=[o,n[o]]}return u}function Ee(n){return Ft(n,zo(n))}function Ce(n){return(n=u(n))&&n.replace(En,a).replace(xn,"")}function Ue(n,t){var r="";if(n=u(n),t=+t,1>t||!n||!mu(t))return r;do t%2&&(r+=n),t=yu(t/2),n+=n;while(t);return r}function We(n,t,r){var e=n;return(n=u(n))?(r?Ur(e,t,r):null==t)?n.slice(g(n),y(n)+1):(t+="",n.slice(o(n,t),i(n,t)+1)):n}function $e(n,t,r){return r&&Ur(n,t,r)&&(t=w),n=u(n),n.match(t||Wn)||[]}function Se(n,t,r){return r&&Ur(n,t,r)&&(t=w),h(n)?Ne(n):ut(n,t)}function Fe(n){return n}function Ne(n){return bt(ot(n,true))}function Te(n,t,r){if(null==r){var e=ge(t),u=e?zo(t):w;((u=u&&u.length?gt(t,u):w)?u.length:e)||(u=false,r=t,t=n,n=this)}u||(u=gt(t,zo(t)));var o=true,e=-1,i=ve(n),f=u.length;false===r?o=false:ge(r)&&"chain"in r&&(o=r.chain);for(;++e<f;){r=u[e];var a=t[r];n[r]=a,i&&(n.prototype[r]=function(t){return function(){var r=this.__chain__;if(o||r){var e=n(this.__wrapped__);return(e.__actions__=qn(this.__actions__)).push({func:t,args:arguments,thisArg:n}),e.__chain__=r,e}return t.apply(n,Jn([this.value()],arguments));}}(a))}return n}function Le(){}function ze(n){return Wr(n)?jt(n):kt(n)}_=_?Yn.defaults(Zn.Object(),_,Yn.pick(Zn,$n)):Zn;var Be=_.Array,De=_.Date,Me=_.Error,qe=_.Function,Pe=_.Math,Ke=_.Number,Ve=_.Object,Ze=_.RegExp,Ye=_.String,Ge=_.TypeError,Je=Be.prototype,Xe=Ve.prototype,He=Ye.prototype,Qe=qe.prototype.toString,nu=Xe.hasOwnProperty,tu=0,ru=Xe.toString,eu=Zn._,uu=Ze("^"+Qe.call(nu).replace(/[\\^$.*+?()[\]{}|]/g,"\\$&").replace(/hasOwnProperty|(function).*?(?=\\\()| for .+?(?=\\\])/g,"$1.*?")+"$"),ou=_.ArrayBuffer,iu=_.clearTimeout,fu=_.parseFloat,au=Pe.pow,cu=Xe.propertyIsEnumerable,lu=jr(_,"Set"),su=_.setTimeout,pu=Je.splice,hu=_.Uint8Array,_u=jr(_,"WeakMap"),vu=Pe.ceil,gu=jr(Ve,"create"),yu=Pe.floor,du=jr(Be,"isArray"),mu=_.isFinite,wu=jr(Ve,"keys"),bu=Pe.max,xu=Pe.min,Au=jr(De,"now"),ju=_.parseInt,ku=Pe.random,Iu=Ke.NEGATIVE_INFINITY,Ru=Ke.POSITIVE_INFINITY,Ou=4294967294,Eu=2147483647,Cu=9007199254740991,Uu=_u&&new _u,Wu={};Nn.support={},Nn.templateSettings={escape:_n,evaluate:vn,interpolate:gn,variable:"",imports:{_:Nn}};var $u=function(){function n(){}return function(t){if(ge(t)){n.prototype=t;var r=new n;n.prototype=w}return r||{}}}(),Su=Vt(_t),Fu=Vt(vt,true),Nu=Zt(),Tu=Zt(true),Lu=Uu?function(n,t){return Uu.set(n,t),n}:Fe,zu=Uu?function(n){return Uu.get(n)}:Le,Bu=jt("length"),Du=function(){var n=0,t=0;return function(r,e){var u=ho(),o=S-(u-t);if(t=u,0<o){if(++n>=$)return r}else n=0;return Lu(r,e)}}(),Mu=le(function(n,t){return h(n)&&Er(n)?ft(n,pt(t,false,true)):[]}),qu=tr(),Pu=tr(true),Ku=le(function(n){for(var t=n.length,e=t,u=Be(l),o=xr(),i=o===r,f=[];e--;){var a=n[e]=Er(a=n[e])?a:[];u[e]=i&&120<=a.length&&gu&&lu?new Dn(e&&a):null}var i=n[0],c=-1,l=i?i.length:0,s=u[0];n:for(;++c<l;)if(a=i[c],0>(s?Mn(s,a):o(f,a,0))){for(e=t;--e;){var p=u[e];if(0>(p?Mn(p,a):o(n[e],a,0)))continue n}s&&s.push(a),f.push(a)}return f}),Vu=le(function(t,r){r=pt(r);var e=rt(t,r);return It(t,r.sort(n)),e}),Zu=vr(),Yu=vr(true),Gu=le(function(n){return St(pt(n,false,true));}),Ju=le(function(n,t){return Er(n)?ft(n,t):[]}),Xu=le(Jr),Hu=le(function(n){var t=n.length,r=2<t?n[t-2]:w,e=1<t?n[t-1]:w;return 2<t&&typeof r=="function"?t-=2:(r=1<t&&typeof e=="function"?(--t,e):w,e=w),n.length=t,Xr(n,r,e)}),Qu=le(function(n){return n=pt(n),this.thru(function(t){t=Oo(t)?t:[Br(t)];for(var r=n,e=-1,u=t.length,o=-1,i=r.length,f=Be(u+i);++e<u;)f[e]=t[e];for(;++o<i;)f[e++]=r[o];return f})}),no=le(function(n,t){return rt(n,pt(t))}),to=Pt(function(n,t,r){nu.call(n,r)?++n[r]:n[r]=1}),ro=nr(Su),eo=nr(Fu,true),uo=ur(Pn,Su),oo=ur(function(n,t){for(var r=n.length;r--&&false!==t(n[r],r,n););return n},Fu),io=Pt(function(n,t,r){nu.call(n,r)?n[r].push(t):n[r]=[t]}),fo=Pt(function(n,t,r){n[r]=t}),ao=le(function(n,t,r){var e=-1,u=typeof t=="function",o=Wr(t),i=Er(n)?Be(n.length):[];return Su(n,function(n){var f=u?t:o&&null!=n?n[t]:w;i[++e]=f?f.apply(n,r):Or(n,t,r)}),i}),co=Pt(function(n,t,r){n[r?0:1].push(t)},function(){return[[],[]]}),lo=lr(Xn,Su),so=lr(function(n,t,r,e){var u=n.length;for(e&&u&&(r=n[--u]);u--;)r=t(r,n[u],u,n);return r},Fu),po=le(function(n,t){if(null==n)return[];var r=t[2];return r&&Ur(t[0],t[1],r)&&(t.length=1),Wt(n,pt(t),[])}),ho=Au||function(){return(new De).getTime()},_o=le(function(n,t,r){var e=x;if(r.length)var u=v(r,_o.placeholder),e=e|R;return gr(n,e,t,r,u)}),vo=le(function(n,t){t=t.length?pt(t):Ie(n);for(var r=-1,e=t.length;++r<e;){var u=t[r];n[u]=gr(n[u],x,n)}return n}),go=le(function(n,t,r){var e=x|A;if(r.length)var u=v(r,go.placeholder),e=e|R;return gr(t,e,n,r,u)}),yo=Xt(k),mo=Xt(I),wo=le(function(n,t){return it(n,1,t)}),bo=le(function(n,t,r){return it(n,t,r)}),xo=er(),Ao=er(true),jo=le(function(n,t){if(t=pt(t),typeof n!="function"||!Kn(t,e))throw new Ge(L);var r=t.length;return le(function(e){for(var u=xu(e.length,r);u--;)e[u]=t[u](e[u]);return n.apply(this,e)})}),ko=cr(R),Io=cr(O),Ro=le(function(n,t){return gr(n,C,w,w,w,pt(t))}),Oo=du||function(n){return h(n)&&Sr(n.length)&&ru.call(n)==D},Eo=Kt(At),Co=Kt(function(n,t,r){return r?nt(n,t,r):tt(n,t)}),Uo=Ht(Co,function(n,t){return n===w?t:n}),Wo=Ht(Eo,Fr),$o=rr(_t),So=rr(vt),Fo=or(Nu),No=or(Tu),To=ir(_t),Lo=ir(vt),zo=wu?function(n){var t=null==n?w:n.constructor;return typeof t=="function"&&t.prototype===n||typeof n!="function"&&Er(n)?Lr(n):ge(n)?wu(n):[]}:Lr,Bo=fr(true),Do=fr(),Mo=le(function(n,t){if(null==n)return{};if("function"!=typeof t[0])return t=Gn(pt(t),Ye),Nr(n,ft(Re(n),t));var r=Bt(t[0],t[1],3);return Tr(n,function(n,t,e){return!r(n,t,e)})}),qo=le(function(n,t){return null==n?{}:"function"==typeof t[0]?Tr(n,Bt(t[0],t[1],3)):Nr(n,pt(t))}),Po=Gt(function(n,t,r){return t=t.toLowerCase(),n+(r?t.charAt(0).toUpperCase()+t.slice(1):t);}),Ko=Gt(function(n,t,r){return n+(r?"-":"")+t.toLowerCase()}),Vo=ar(),Zo=ar(true),Yo=Gt(function(n,t,r){return n+(r?"_":"")+t.toLowerCase()}),Go=Gt(function(n,t,r){return n+(r?" ":"")+(t.charAt(0).toUpperCase()+t.slice(1))}),Jo=le(function(n,t){try{return n.apply(w,t)}catch(r){return _e(r)?r:new Me(r)}}),Xo=le(function(n,t){return function(r){return Or(r,n,t)}}),Ho=le(function(n,t){return function(r){return Or(n,r,t)}}),Qo=_r("ceil"),ni=_r("floor"),ti=Qt(se,Iu),ri=Qt(Ae,Ru),ei=_r("round");return Nn.prototype=Tn.prototype,Ln.prototype=$u(Tn.prototype),Ln.prototype.constructor=Ln,zn.prototype=$u(Tn.prototype),zn.prototype.constructor=zn,Bn.prototype["delete"]=function(n){return this.has(n)&&delete this.__data__[n]},Bn.prototype.get=function(n){return"__proto__"==n?w:this.__data__[n]},Bn.prototype.has=function(n){return"__proto__"!=n&&nu.call(this.__data__,n)},Bn.prototype.set=function(n,t){return"__proto__"!=n&&(this.__data__[n]=t),this},Dn.prototype.push=function(n){var t=this.data;typeof n=="string"||ge(n)?t.set.add(n):t.hash[n]=true;},ce.Cache=Bn,Nn.after=function(n,t){if(typeof t!="function"){if(typeof n!="function")throw new Ge(L);var r=n;n=t,t=r}return n=mu(n=+n)?n:0,function(){return 1>--n?t.apply(this,arguments):void 0}},Nn.ary=function(n,t,r){return r&&Ur(n,t,r)&&(t=w),t=n&&null==t?n.length:bu(+t||0,0),gr(n,E,w,w,w,w,t)},Nn.assign=Co,Nn.at=no,Nn.before=fe,Nn.bind=_o,Nn.bindAll=vo,Nn.bindKey=go,Nn.callback=Se,Nn.chain=Qr,Nn.chunk=function(n,t,r){t=(r?Ur(n,t,r):null==t)?1:bu(yu(t)||1,1),r=0;for(var e=n?n.length:0,u=-1,o=Be(vu(e/t));r<e;)o[++u]=Et(n,r,r+=t);return o},Nn.compact=function(n){for(var t=-1,r=n?n.length:0,e=-1,u=[];++t<r;){var o=n[t];o&&(u[++e]=o)}return u},Nn.constant=function(n){return function(){return n}},Nn.countBy=to,Nn.create=function(n,t,r){var e=$u(n);return r&&Ur(n,t,r)&&(t=w),t?tt(e,t):e},Nn.curry=yo,Nn.curryRight=mo,Nn.debounce=ae,Nn.defaults=Uo,Nn.defaultsDeep=Wo,Nn.defer=wo,Nn.delay=bo,Nn.difference=Mu,Nn.drop=qr,Nn.dropRight=Pr,Nn.dropRightWhile=function(n,t,r){return n&&n.length?Nt(n,wr(t,r,3),true,true):[]},Nn.dropWhile=function(n,t,r){return n&&n.length?Nt(n,wr(t,r,3),true):[]},Nn.fill=function(n,t,r,e){var u=n?n.length:0;if(!u)return[];for(r&&typeof r!="number"&&Ur(n,t,r)&&(r=0,e=u),u=n.length,r=null==r?0:+r||0,0>r&&(r=-r>u?0:u+r),e=e===w||e>u?u:+e||0,0>e&&(e+=u),u=r>e?0:e>>>0,r>>>=0;r<u;)n[r++]=t;return n},Nn.filter=re,Nn.flatten=function(n,t,r){var e=n?n.length:0;return r&&Ur(n,t,r)&&(t=false),e?pt(n,t):[]},Nn.flattenDeep=function(n){return n&&n.length?pt(n,true):[]},Nn.flow=xo,Nn.flowRight=Ao,Nn.forEach=uo,Nn.forEachRight=oo,Nn.forIn=Fo,Nn.forInRight=No,Nn.forOwn=To,Nn.forOwnRight=Lo,Nn.functions=Ie,Nn.groupBy=io,Nn.indexBy=fo,Nn.initial=function(n){return Pr(n,1)},Nn.intersection=Ku,Nn.invert=function(n,t,r){r&&Ur(n,t,r)&&(t=w),r=-1;for(var e=zo(n),u=e.length,o={};++r<u;){var i=e[r],f=n[i];t?nu.call(o,f)?o[f].push(i):o[f]=[i]:o[f]=i}return o},Nn.invoke=ao,Nn.keys=zo,Nn.keysIn=Re,Nn.map=ue,Nn.mapKeys=Bo,Nn.mapValues=Do,Nn.matches=Ne,Nn.matchesProperty=function(n,t){return xt(n,ot(t,true))},Nn.memoize=ce,Nn.merge=Eo,Nn.method=Xo,Nn.methodOf=Ho,Nn.mixin=Te,Nn.modArgs=jo,Nn.negate=function(n){if(typeof n!="function")throw new Ge(L);return function(){return!n.apply(this,arguments)}},Nn.omit=Mo,Nn.once=function(n){return fe(2,n)},Nn.pairs=Oe,Nn.partial=ko,Nn.partialRight=Io,Nn.partition=co,Nn.pick=qo,Nn.pluck=function(n,t){return ue(n,ze(t))},Nn.property=ze,Nn.propertyOf=function(n){return function(t){return yt(n,Dr(t),t+"")}},Nn.pull=function(){var n=arguments,t=n[0];if(!t||!t.length)return t;for(var r=0,e=xr(),u=n.length;++r<u;)for(var o=0,i=n[r];-1<(o=e(t,i,o));)pu.call(t,o,1);return t},Nn.pullAt=Vu,Nn.range=function(n,t,r){r&&Ur(n,t,r)&&(t=r=w),n=+n||0,r=null==r?1:+r||0,null==t?(t=n,n=0):t=+t||0;var e=-1;t=bu(vu((t-n)/(r||1)),0);for(var u=Be(t);++e<t;)u[e]=n,n+=r;return u},Nn.rearg=Ro,Nn.reject=function(n,t,r){var e=Oo(n)?Vn:lt;return t=wr(t,r,3),e(n,function(n,r,e){return!t(n,r,e)})},Nn.remove=function(n,t,r){var e=[];if(!n||!n.length)return e;var u=-1,o=[],i=n.length;for(t=wr(t,r,3);++u<i;)r=n[u],t(r,u,n)&&(e.push(r),o.push(u));return It(n,o),e},Nn.rest=Yr,Nn.restParam=le,Nn.set=function(n,t,r){if(null==n)return n;var e=t+"";t=null!=n[e]||Wr(t,n)?[e]:Dr(t);for(var e=-1,u=t.length,o=u-1,i=n;null!=i&&++e<u;){var f=t[e];ge(i)&&(e==o?i[f]=r:null==i[f]&&(i[f]=Cr(t[e+1])?[]:{})),i=i[f]}return n},Nn.shuffle=function(n){return oe(n,Ru)},Nn.slice=function(n,t,r){var e=n?n.length:0;return e?(r&&typeof r!="number"&&Ur(n,t,r)&&(t=0,r=e),Et(n,t,r)):[]},Nn.sortBy=function(n,t,r){if(null==n)return[];r&&Ur(n,t,r)&&(t=w);var e=-1;return t=wr(t,r,3),n=wt(n,function(n,r,u){return{a:t(n,r,u),b:++e,c:n}}),Ut(n,f)},Nn.sortByAll=po,Nn.sortByOrder=function(n,t,r,e){return null==n?[]:(e&&Ur(t,r,e)&&(r=w),Oo(t)||(t=null==t?[]:[t]),Oo(r)||(r=null==r?[]:[r]),Wt(n,t,r))},Nn.spread=function(n){if(typeof n!="function")throw new Ge(L);return function(t){return n.apply(this,t)}},Nn.take=function(n,t,r){return n&&n.length?((r?Ur(n,t,r):null==t)&&(t=1),Et(n,0,0>t?0:t)):[]},Nn.takeRight=function(n,t,r){var e=n?n.length:0;return e?((r?Ur(n,t,r):null==t)&&(t=1),t=e-(+t||0),Et(n,0>t?0:t)):[]},Nn.takeRightWhile=function(n,t,r){return n&&n.length?Nt(n,wr(t,r,3),false,true):[]},Nn.takeWhile=function(n,t,r){return n&&n.length?Nt(n,wr(t,r,3)):[]},Nn.tap=function(n,t,r){return t.call(r,n),n},Nn.throttle=function(n,t,r){var e=true,u=true;if(typeof n!="function")throw new Ge(L);return false===r?e=false:ge(r)&&(e="leading"in r?!!r.leading:e,u="trailing"in r?!!r.trailing:u),ae(n,t,{leading:e,maxWait:+t,trailing:u})},Nn.thru=ne,Nn.times=function(n,t,r){if(n=yu(n),1>n||!mu(n))return[];var e=-1,u=Be(xu(n,4294967295));for(t=Bt(t,r,1);++e<n;)4294967295>e?u[e]=t(e):t(e);return u},Nn.toArray=je,Nn.toPlainObject=ke,Nn.transform=function(n,t,r,e){var u=Oo(n)||xe(n);return t=wr(t,e,4),null==r&&(u||ge(n)?(e=n.constructor,r=u?Oo(n)?new e:[]:$u(ve(e)?e.prototype:w)):r={}),(u?Pn:_t)(n,function(n,e,u){return t(r,n,e,u)}),r},Nn.union=Gu,Nn.uniq=Gr,Nn.unzip=Jr,Nn.unzipWith=Xr,Nn.values=Ee,Nn.valuesIn=function(n){return Ft(n,Re(n))},Nn.where=function(n,t){return re(n,bt(t))},Nn.without=Ju,Nn.wrap=function(n,t){return t=null==t?Fe:t,gr(t,R,w,[n],[])},Nn.xor=function(){for(var n=-1,t=arguments.length;++n<t;){var r=arguments[n];if(Er(r))var e=e?Jn(ft(e,r),ft(r,e)):r}return e?St(e):[]},Nn.zip=Xu,Nn.zipObject=Hr,Nn.zipWith=Hu,Nn.backflow=Ao,Nn.collect=ue,Nn.compose=Ao,Nn.each=uo,Nn.eachRight=oo,Nn.extend=Co,Nn.iteratee=Se,Nn.methods=Ie,Nn.object=Hr,Nn.select=re,Nn.tail=Yr,Nn.unique=Gr,Te(Nn,Nn),Nn.add=function(n,t){return(+n||0)+(+t||0)},Nn.attempt=Jo,Nn.camelCase=Po,Nn.capitalize=function(n){return(n=u(n))&&n.charAt(0).toUpperCase()+n.slice(1)},Nn.ceil=Qo,Nn.clone=function(n,t,r,e){return t&&typeof t!="boolean"&&Ur(n,t,r)?t=false:typeof t=="function"&&(e=r,r=t,t=false),typeof r=="function"?ot(n,t,Bt(r,e,3)):ot(n,t)},Nn.cloneDeep=function(n,t,r){return typeof t=="function"?ot(n,true,Bt(t,r,3)):ot(n,true)},Nn.deburr=Ce,Nn.endsWith=function(n,t,r){n=u(n),t+="";var e=n.length;return r=r===w?e:xu(0>r?0:+r||0,e),r-=t.length,0<=r&&n.indexOf(t,r)==r},Nn.escape=function(n){return(n=u(n))&&hn.test(n)?n.replace(sn,c):n},Nn.escapeRegExp=function(n){return(n=u(n))&&bn.test(n)?n.replace(wn,l):n||"(?:)"},Nn.every=te,Nn.find=ro,Nn.findIndex=qu,Nn.findKey=$o,Nn.findLast=eo,Nn.findLastIndex=Pu,Nn.findLastKey=So,Nn.findWhere=function(n,t){return ro(n,bt(t))},Nn.first=Kr,Nn.floor=ni,Nn.get=function(n,t,r){return n=null==n?w:yt(n,Dr(t),t+""),n===w?r:n},Nn.gt=se,Nn.gte=function(n,t){return n>=t},Nn.has=function(n,t){if(null==n)return false;var r=nu.call(n,t);if(!r&&!Wr(t)){if(t=Dr(t),n=1==t.length?n:yt(n,Et(t,0,-1)),null==n)return false;t=Zr(t),r=nu.call(n,t)}return r||Sr(n.length)&&Cr(t,n.length)&&(Oo(n)||pe(n))},Nn.identity=Fe,Nn.includes=ee,Nn.indexOf=Vr,Nn.inRange=function(n,t,r){return t=+t||0,r===w?(r=t,t=0):r=+r||0,n>=xu(t,r)&&n<bu(t,r)},Nn.isArguments=pe,Nn.isArray=Oo,Nn.isBoolean=function(n){return true===n||false===n||h(n)&&ru.call(n)==M},Nn.isDate=function(n){return h(n)&&ru.call(n)==q},Nn.isElement=function(n){return!!n&&1===n.nodeType&&h(n)&&!me(n)},Nn.isEmpty=function(n){return null==n?true:Er(n)&&(Oo(n)||be(n)||pe(n)||h(n)&&ve(n.splice))?!n.length:!zo(n).length},Nn.isEqual=he,Nn.isError=_e,Nn.isFinite=function(n){return typeof n=="number"&&mu(n)},Nn.isFunction=ve,Nn.isMatch=function(n,t,r,e){return r=typeof r=="function"?Bt(r,e,3):w,mt(n,Ar(t),r)},Nn.isNaN=function(n){return de(n)&&n!=+n},Nn.isNative=ye,Nn.isNull=function(n){return null===n},Nn.isNumber=de,Nn.isObject=ge,Nn.isPlainObject=me,Nn.isRegExp=we,Nn.isString=be,Nn.isTypedArray=xe,Nn.isUndefined=function(n){return n===w},Nn.kebabCase=Ko,Nn.last=Zr,Nn.lastIndexOf=function(n,t,r){var e=n?n.length:0;if(!e)return-1;var u=e;if(typeof r=="number")u=(0>r?bu(e+r,0):xu(r||0,e-1))+1;else if(r)return u=Lt(n,t,true)-1,n=n[u],(t===t?t===n:n!==n)?u:-1;if(t!==t)return p(n,u,true);for(;u--;)if(n[u]===t)return u;return-1},Nn.lt=Ae,Nn.lte=function(n,t){return n<=t},Nn.max=ti,Nn.min=ri,Nn.noConflict=function(){return Zn._=eu,this},Nn.noop=Le,Nn.now=ho,Nn.pad=function(n,t,r){n=u(n),t=+t;var e=n.length;return e<t&&mu(t)?(e=(t-e)/2,t=yu(e),e=vu(e),r=pr("",e,r),r.slice(0,t)+n+r):n},Nn.padLeft=Vo,Nn.padRight=Zo,Nn.parseInt=function(n,t,r){return(r?Ur(n,t,r):null==t)?t=0:t&&(t=+t),n=We(n),ju(n,t||(In.test(n)?16:10))},Nn.random=function(n,t,r){r&&Ur(n,t,r)&&(t=r=w);var e=null==n,u=null==t;return null==r&&(u&&typeof n=="boolean"?(r=n,n=1):typeof t=="boolean"&&(r=t,u=true)),e&&u&&(t=1,u=false),n=+n||0,u?(t=n,n=0):t=+t||0,r||n%1||t%1?(r=ku(),xu(n+r*(t-n+fu("1e-"+((r+"").length-1))),t)):Rt(n,t)},Nn.reduce=lo,Nn.reduceRight=so,Nn.repeat=Ue,Nn.result=function(n,t,r){var e=null==n?w:n[t];return e===w&&(null==n||Wr(t,n)||(t=Dr(t),n=1==t.length?n:yt(n,Et(t,0,-1)),e=null==n?w:n[Zr(t)]),e=e===w?r:e),ve(e)?e.call(n):e},Nn.round=ei,Nn.runInContext=m,Nn.size=function(n){var t=n?Bu(n):0;return Sr(t)?t:zo(n).length},Nn.snakeCase=Yo,Nn.some=ie,Nn.sortedIndex=Zu,Nn.sortedLastIndex=Yu,Nn.startCase=Go,Nn.startsWith=function(n,t,r){return n=u(n),r=null==r?0:xu(0>r?0:+r||0,n.length),n.lastIndexOf(t,r)==r},Nn.sum=function(n,t,r){if(r&&Ur(n,t,r)&&(t=w),t=wr(t,r,3),1==t.length){n=Oo(n)?n:zr(n),r=n.length;for(var e=0;r--;)e+=+t(n[r])||0;n=e}else n=$t(n,t);return n},Nn.template=function(n,t,r){var e=Nn.templateSettings;r&&Ur(n,t,r)&&(t=r=w),n=u(n),t=nt(tt({},r||t),e,Qn),r=nt(tt({},t.imports),e.imports,Qn);var o,i,f=zo(r),a=Ft(r,f),c=0;r=t.interpolate||Cn;var l="__p+='";r=Ze((t.escape||Cn).source+"|"+r.source+"|"+(r===gn?jn:Cn).source+"|"+(t.evaluate||Cn).source+"|$","g");var p="sourceURL"in t?"//# sourceURL="+t.sourceURL+"\n":"";if(n.replace(r,function(t,r,e,u,f,a){return e||(e=u),l+=n.slice(c,a).replace(Un,s),r&&(o=true,l+="'+__e("+r+")+'"),f&&(i=true,l+="';"+f+";\n__p+='"),e&&(l+="'+((__t=("+e+"))==null?'':__t)+'"),c=a+t.length,t}),l+="';",(t=t.variable)||(l="with(obj){"+l+"}"),l=(i?l.replace(fn,""):l).replace(an,"$1").replace(cn,"$1;"),l="function("+(t||"obj")+"){"+(t?"":"obj||(obj={});")+"var __t,__p=''"+(o?",__e=_.escape":"")+(i?",__j=Array.prototype.join;function print(){__p+=__j.call(arguments,'')}":";")+l+"return __p}",t=Jo(function(){return qe(f,p+"return "+l).apply(w,a)}),t.source=l,_e(t))throw t;return t},Nn.trim=We,Nn.trimLeft=function(n,t,r){var e=n;return(n=u(n))?n.slice((r?Ur(e,t,r):null==t)?g(n):o(n,t+"")):n},Nn.trimRight=function(n,t,r){var e=n;return(n=u(n))?(r?Ur(e,t,r):null==t)?n.slice(0,y(n)+1):n.slice(0,i(n,t+"")+1):n;},Nn.trunc=function(n,t,r){r&&Ur(n,t,r)&&(t=w);var e=U;if(r=W,null!=t)if(ge(t)){var o="separator"in t?t.separator:o,e="length"in t?+t.length||0:e;r="omission"in t?u(t.omission):r}else e=+t||0;if(n=u(n),e>=n.length)return n;if(e-=r.length,1>e)return r;if(t=n.slice(0,e),null==o)return t+r;if(we(o)){if(n.slice(e).search(o)){var i,f=n.slice(0,e);for(o.global||(o=Ze(o.source,(kn.exec(o)||"")+"g")),o.lastIndex=0;n=o.exec(f);)i=n.index;t=t.slice(0,null==i?e:i)}}else n.indexOf(o,e)!=e&&(o=t.lastIndexOf(o),-1<o&&(t=t.slice(0,o)));return t+r},Nn.unescape=function(n){return(n=u(n))&&pn.test(n)?n.replace(ln,d):n},Nn.uniqueId=function(n){var t=++tu;return u(n)+t},Nn.words=$e,Nn.all=te,Nn.any=ie,Nn.contains=ee,Nn.eq=he,Nn.detect=ro,Nn.foldl=lo,Nn.foldr=so,Nn.head=Kr,Nn.include=ee,Nn.inject=lo,Te(Nn,function(){var n={};return _t(Nn,function(t,r){Nn.prototype[r]||(n[r]=t)}),n}(),false),Nn.sample=oe,Nn.prototype.sample=function(n){return this.__chain__||null!=n?this.thru(function(t){return oe(t,n)}):oe(this.value());},Nn.VERSION=b,Pn("bind bindKey curry curryRight partial partialRight".split(" "),function(n){Nn[n].placeholder=Nn}),Pn(["drop","take"],function(n,t){zn.prototype[n]=function(r){var e=this.__filtered__;if(e&&!t)return new zn(this);r=null==r?1:bu(yu(r)||0,0);var u=this.clone();return e?u.__takeCount__=xu(u.__takeCount__,r):u.__views__.push({size:r,type:n+(0>u.__dir__?"Right":"")}),u},zn.prototype[n+"Right"]=function(t){return this.reverse()[n](t).reverse()}}),Pn(["filter","map","takeWhile"],function(n,t){var r=t+1,e=r!=T;zn.prototype[n]=function(n,t){var u=this.clone();return u.__iteratees__.push({iteratee:wr(n,t,1),type:r}),u.__filtered__=u.__filtered__||e,u}}),Pn(["first","last"],function(n,t){var r="take"+(t?"Right":"");zn.prototype[n]=function(){return this[r](1).value()[0]}}),Pn(["initial","rest"],function(n,t){var r="drop"+(t?"":"Right");zn.prototype[n]=function(){return this.__filtered__?new zn(this):this[r](1)}}),Pn(["pluck","where"],function(n,t){var r=t?"filter":"map",e=t?bt:ze;zn.prototype[n]=function(n){return this[r](e(n))}}),zn.prototype.compact=function(){return this.filter(Fe)},zn.prototype.reject=function(n,t){return n=wr(n,t,1),this.filter(function(t){return!n(t)})},zn.prototype.slice=function(n,t){n=null==n?0:+n||0;var r=this;return r.__filtered__&&(0<n||0>t)?new zn(r):(0>n?r=r.takeRight(-n):n&&(r=r.drop(n)),t!==w&&(t=+t||0,r=0>t?r.dropRight(-t):r.take(t-n)),r)},zn.prototype.takeRightWhile=function(n,t){return this.reverse().takeWhile(n,t).reverse()},zn.prototype.toArray=function(){return this.take(Ru);},_t(zn.prototype,function(n,t){var r=/^(?:filter|map|reject)|While$/.test(t),e=/^(?:first|last)$/.test(t),u=Nn[e?"take"+("last"==t?"Right":""):t];u&&(Nn.prototype[t]=function(){function t(n){return e&&i?u(n,1)[0]:u.apply(w,Jn([n],o))}var o=e?[1]:arguments,i=this.__chain__,f=this.__wrapped__,a=!!this.__actions__.length,c=f instanceof zn,l=o[0],s=c||Oo(f);return s&&r&&typeof l=="function"&&1!=l.length&&(c=s=false),l={func:ne,args:[t],thisArg:w},a=c&&!a,e&&!i?a?(f=f.clone(),f.__actions__.push(l),n.call(f)):u.call(w,this.value())[0]:!e&&s?(f=a?f:new zn(this),f=n.apply(f,o),f.__actions__.push(l),new Ln(f,i)):this.thru(t)})}),Pn("join pop push replace shift sort splice split unshift".split(" "),function(n){var t=(/^(?:replace|split)$/.test(n)?He:Je)[n],r=/^(?:push|sort|unshift)$/.test(n)?"tap":"thru",e=/^(?:join|pop|replace|shift)$/.test(n);Nn.prototype[n]=function(){var n=arguments;return e&&!this.__chain__?t.apply(this.value(),n):this[r](function(r){return t.apply(r,n)})}}),_t(zn.prototype,function(n,t){var r=Nn[t];if(r){var e=r.name+"";(Wu[e]||(Wu[e]=[])).push({name:t,func:r})}}),Wu[sr(w,A).name]=[{name:"wrapper",func:w}],zn.prototype.clone=function(){var n=new zn(this.__wrapped__);return n.__actions__=qn(this.__actions__),n.__dir__=this.__dir__,n.__filtered__=this.__filtered__,n.__iteratees__=qn(this.__iteratees__),n.__takeCount__=this.__takeCount__,n.__views__=qn(this.__views__),n},zn.prototype.reverse=function(){if(this.__filtered__){var n=new zn(this);n.__dir__=-1,n.__filtered__=true}else n=this.clone(),n.__dir__*=-1;return n},zn.prototype.value=function(){var n,t=this.__wrapped__.value(),r=this.__dir__,e=Oo(t),u=0>r,o=e?t.length:0;n=o;for(var i=this.__views__,f=0,a=-1,c=i.length;++a<c;){var l=i[a],s=l.size;switch(l.type){case"drop":f+=s;break;case"dropRight":n-=s;break;case"take":n=xu(n,f+s);break;case"takeRight":f=bu(f,n-s)}}if(n={start:f,end:n},i=n.start,f=n.end,n=f-i,u=u?f:i-1,i=this.__iteratees__,f=i.length,a=0,c=xu(n,this.__takeCount__),!e||o<F||o==n&&c==n)return Tt(t,this.__actions__);e=[];n:for(;n--&&a<c;){for(u+=r,o=-1,l=t[u];++o<f;){var p=i[o],s=p.type,p=p.iteratee(l);if(s==T)l=p;else if(!p){if(s==N)continue n;break n}}e[a++]=l}return e},Nn.prototype.chain=function(){return Qr(this)},Nn.prototype.commit=function(){return new Ln(this.value(),this.__chain__)},Nn.prototype.concat=Qu,Nn.prototype.plant=function(n){for(var t,r=this;r instanceof Tn;){var e=Mr(r);t?u.__wrapped__=e:t=e;var u=e,r=r.__wrapped__}return u.__wrapped__=n,t},Nn.prototype.reverse=function(){function n(n){return n.reverse()}var t=this.__wrapped__;return t instanceof zn?(this.__actions__.length&&(t=new zn(this)),t=t.reverse(),t.__actions__.push({func:ne,args:[n],thisArg:w}),new Ln(t,this.__chain__)):this.thru(n)},Nn.prototype.toString=function(){return this.value()+""},Nn.prototype.run=Nn.prototype.toJSON=Nn.prototype.valueOf=Nn.prototype.value=function(){return Tt(this.__wrapped__,this.__actions__)},Nn.prototype.collect=Nn.prototype.map,Nn.prototype.head=Nn.prototype.first,Nn.prototype.select=Nn.prototype.filter,Nn.prototype.tail=Nn.prototype.rest,Nn}var w,b="3.10.1",x=1,A=2,j=4,k=8,I=16,R=32,O=64,E=128,C=256,U=30,W="...",$=150,S=16,F=200,N=1,T=2,L="Expected a function",z="__lodash_placeholder__",B="[object Arguments]",D="[object Array]",M="[object Boolean]",q="[object Date]",P="[object Error]",K="[object Function]",V="[object Number]",Z="[object Object]",Y="[object RegExp]",G="[object String]",J="[object ArrayBuffer]",X="[object Float32Array]",H="[object Float64Array]",Q="[object Int8Array]",nn="[object Int16Array]",tn="[object Int32Array]",rn="[object Uint8Array]",en="[object Uint8ClampedArray]",un="[object Uint16Array]",on="[object Uint32Array]",fn=/\b__p\+='';/g,an=/\b(__p\+=)''\+/g,cn=/(__e\(.*?\)|\b__t\))\+'';/g,ln=/&(?:amp|lt|gt|quot|#39|#96);/g,sn=/[&<>"'`]/g,pn=RegExp(ln.source),hn=RegExp(sn.source),_n=/<%-([\s\S]+?)%>/g,vn=/<%([\s\S]+?)%>/g,gn=/<%=([\s\S]+?)%>/g,yn=/\.|\[(?:[^[\]]*|(["'])(?:(?!\1)[^\n\\]|\\.)*?\1)\]/,dn=/^\w*$/,mn=/[^.[\]]+|\[(?:(-?\d+(?:\.\d+)?)|(["'])((?:(?!\2)[^\n\\]|\\.)*?)\2)\]/g,wn=/^[:!,]|[\\^$.*+?()[\]{}|\/]|(^[0-9a-fA-Fnrtuvx])|([\n\r\u2028\u2029])/g,bn=RegExp(wn.source),xn=/[\u0300-\u036f\ufe20-\ufe23]/g,An=/\\(\\)?/g,jn=/\$\{([^\\}]*(?:\\.[^\\}]*)*)\}/g,kn=/\w*$/,In=/^0[xX]/,Rn=/^\[object .+?Constructor\]$/,On=/^\d+$/,En=/[\xc0-\xd6\xd8-\xde\xdf-\xf6\xf8-\xff]/g,Cn=/($^)/,Un=/['\n\r\u2028\u2029\\]/g,Wn=RegExp("[A-Z\\xc0-\\xd6\\xd8-\\xde]+(?=[A-Z\\xc0-\\xd6\\xd8-\\xde][a-z\\xdf-\\xf6\\xf8-\\xff]+)|[A-Z\\xc0-\\xd6\\xd8-\\xde]?[a-z\\xdf-\\xf6\\xf8-\\xff]+|[A-Z\\xc0-\\xd6\\xd8-\\xde]+|[0-9]+","g"),$n="Array ArrayBuffer Date Error Float32Array Float64Array Function Int8Array Int16Array Int32Array Math Number Object RegExp Set String _ clearTimeout isFinite parseFloat parseInt setTimeout TypeError Uint8Array Uint8ClampedArray Uint16Array Uint32Array WeakMap".split(" "),Sn={};Sn[X]=Sn[H]=Sn[Q]=Sn[nn]=Sn[tn]=Sn[rn]=Sn[en]=Sn[un]=Sn[on]=true,Sn[B]=Sn[D]=Sn[J]=Sn[M]=Sn[q]=Sn[P]=Sn[K]=Sn["[object Map]"]=Sn[V]=Sn[Z]=Sn[Y]=Sn["[object Set]"]=Sn[G]=Sn["[object WeakMap]"]=false;var Fn={};Fn[B]=Fn[D]=Fn[J]=Fn[M]=Fn[q]=Fn[X]=Fn[H]=Fn[Q]=Fn[nn]=Fn[tn]=Fn[V]=Fn[Z]=Fn[Y]=Fn[G]=Fn[rn]=Fn[en]=Fn[un]=Fn[on]=true,Fn[P]=Fn[K]=Fn["[object Map]"]=Fn["[object Set]"]=Fn["[object WeakMap]"]=false;var Nn={"\xc0":"A","\xc1":"A","\xc2":"A","\xc3":"A","\xc4":"A","\xc5":"A","\xe0":"a","\xe1":"a","\xe2":"a","\xe3":"a","\xe4":"a","\xe5":"a","\xc7":"C","\xe7":"c","\xd0":"D","\xf0":"d","\xc8":"E","\xc9":"E","\xca":"E","\xcb":"E","\xe8":"e","\xe9":"e","\xea":"e","\xeb":"e","\xcc":"I","\xcd":"I","\xce":"I","\xcf":"I","\xec":"i","\xed":"i","\xee":"i","\xef":"i","\xd1":"N","\xf1":"n","\xd2":"O","\xd3":"O","\xd4":"O","\xd5":"O","\xd6":"O","\xd8":"O","\xf2":"o","\xf3":"o","\xf4":"o","\xf5":"o","\xf6":"o","\xf8":"o","\xd9":"U","\xda":"U","\xdb":"U","\xdc":"U","\xf9":"u","\xfa":"u","\xfb":"u","\xfc":"u","\xdd":"Y","\xfd":"y","\xff":"y","\xc6":"Ae","\xe6":"ae","\xde":"Th","\xfe":"th","\xdf":"ss"},Tn={"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;","`":"&#96;"},Ln={"&amp;":"&","&lt;":"<","&gt;":">","&quot;":'"',"&#39;":"'","&#96;":"`"},zn={"function":true,object:true},Bn={0:"x30",1:"x31",2:"x32",3:"x33",4:"x34",5:"x35",6:"x36",7:"x37",8:"x38",9:"x39",A:"x41",B:"x42",C:"x43",D:"x44",E:"x45",F:"x46",a:"x61",b:"x62",c:"x63",d:"x64",e:"x65",f:"x66",n:"x6e",r:"x72",t:"x74",u:"x75",v:"x76",x:"x78"},Dn={"\\":"\\","'":"'","\n":"n","\r":"r","\u2028":"u2028","\u2029":"u2029"},Mn=zn[typeof exports]&&exports&&!exports.nodeType&&exports,qn=zn[typeof module]&&module&&!module.nodeType&&module,Pn=zn[typeof self]&&self&&self.Object&&self,Kn=zn[typeof window]&&window&&window.Object&&window,Vn=qn&&qn.exports===Mn&&Mn,Zn=Mn&&qn&&typeof global=="object"&&global&&global.Object&&global||Kn!==(this&&this.window)&&Kn||Pn||this,Yn=m();typeof define=="function"&&typeof define.amd=="object"&&define.amd?(Zn._=Yn, define(function(){return Yn})):Mn&&qn?Vn?(qn.exports=Yn)._=Yn:Mn._=Yn:Zn._=Yn}).call(this);


////////////////////////////////////////////////////////////////////////////////
//// Function for boxplot
////////////////////////////////////////////////////////////////////////////////

d3.box = function() {
  var width = 1,
      height = 1,
      duration = 0,
      domain = null,
      value = Number,
      whiskers = boxWhiskers,
      quartiles = boxQuartiles,
      tickFormat = null;

  // For each small multiple…
  function box(g) {
    g.each(function(d, i) {
      d = d.map(value).sort(d3.ascending);
      var g = d3.select(this),
          n = d.length,
          min = d[0],
          max = d[n - 1];

      // Compute quartiles. Must return exactly 3 elements.
      var quartileData = d.quartiles = quartiles(d);

      // Compute whiskers. Must return exactly 2 elements, or null.
      var whiskerIndices = whiskers && whiskers.call(this, d, i),
          whiskerData = whiskerIndices && whiskerIndices.map(function(i) { return d[i]; });

      // Compute outliers. If no whiskers are specified, all data are "outliers".
      // We compute the outliers as indices, so that we can join across transitions!
      var outlierIndices = whiskerIndices
          ? d3.range(0, whiskerIndices[0]).concat(d3.range(whiskerIndices[1] + 1, n))
          : d3.range(n);

      // Compute the new x-scale.
      var x1 = d3.scale.linear()
          .domain(domain && domain.call(this, d, i) || [min, max])
          .range([height, 0]);

      // Retrieve the old x-scale, if this is an update.
      var x0 = this.__chart__ || d3.scale.linear()
          .domain([0, Infinity])
          .range(x1.range());

      // Stash the new scale.
      this.__chart__ = x1;

      // Note: the box, median, and box tick elements are fixed in number,
      // so we only have to handle enter and update. In contrast, the outliers
      // and other elements are variable, so we need to exit them! Variable
      // elements also fade in and out.

      // Update center line: the vertical line spanning the whiskers.
      var center = g.selectAll("line.center")
          .data(whiskerData ? [whiskerData] : []);

      center.enter().insert("line", "rect")
          .attr("class", "center")
          .attr("x1", width / 2)
          .attr("y1", function(d) { return x0(d[0]); })
          .attr("x2", width / 2)
          .attr("y2", function(d) { return x0(d[1]); })
          .style("opacity", 1e-6)
        .transition()
          .duration(duration)
          .style("opacity", 1)
          .attr("y1", function(d) { return x1(d[0]); })
          .attr("y2", function(d) { return x1(d[1]); });

      center.transition()
          .duration(duration)
          .style("opacity", 1)
          .attr("y1", function(d) { return x1(d[0]); })
          .attr("y2", function(d) { return x1(d[1]); });

      center.exit().transition()
          .duration(duration)
          .style("opacity", 1e-6)
          .attr("y1", function(d) { return x1(d[0]); })
          .attr("y2", function(d) { return x1(d[1]); })
          .remove();

      // Update innerquartile box.
      var box = g.selectAll("rect.box")
          .data([quartileData]);

      box.enter().append("rect")
          .attr("class", "box")
          .attr("x", 0)
          .attr("y", function(d) { return x0(d[2]); })
          .attr("width", width)
          .attr("height", function(d) { return x0(d[0]) - x0(d[2]); })
        .transition()
          .duration(duration)
          .attr("y", function(d) { return x1(d[2]); })
          .attr("height", function(d) { return x1(d[0]) - x1(d[2]); });

      box.transition()
          .duration(duration)
          .attr("y", function(d) { return x1(d[2]); })
          .attr("height", function(d) { return x1(d[0]) - x1(d[2]); });

      // Update median line.
      var medianLine = g.selectAll("line.median")
          .data([quartileData[1]]);

      medianLine.enter().append("line")
          .attr("class", "median")
          .attr("x1", 0)
          .attr("y1", x0)
          .attr("x2", width)
          .attr("y2", x0)
        .transition()
          .duration(duration)
          .attr("y1", x1)
          .attr("y2", x1);

      medianLine.transition()
          .duration(duration)
          .attr("y1", x1)
          .attr("y2", x1);

      // Update whiskers.
      var whisker = g.selectAll("line.whisker")
          .data(whiskerData || []);

      whisker.enter().insert("line", "circle, text")
          .attr("class", "whisker")
          .attr("x1", 0)
          .attr("y1", x0)
          .attr("x2", width)
          .attr("y2", x0)
          .style("opacity", 1e-6)
        .transition()
          .duration(duration)
          .attr("y1", x1)
          .attr("y2", x1)
          .style("opacity", 1);

      whisker.transition()
          .duration(duration)
          .attr("y1", x1)
          .attr("y2", x1)
          .style("opacity", 1);

      whisker.exit().transition()
          .duration(duration)
          .attr("y1", x1)
          .attr("y2", x1)
          .style("opacity", 1e-6)
          .remove();

      // Update outliers.
      var outlier = g.selectAll("circle.outlier")
          .data(outlierIndices, Number);

      outlier.enter().insert("circle", "text")
          .attr("class", "outlier")
          .attr("r", 5)
          .attr("cx", width / 2)
          .attr("cy", function(i) { return x0(d[i]); })
          .style("opacity", 1e-6)
        .transition()
          .duration(duration)
          .attr("cy", function(i) { return x1(d[i]); })
          .style("opacity", 1);

      outlier.transition()
          .duration(duration)
          .attr("cy", function(i) { return x1(d[i]); })
          .style("opacity", 1);

      outlier.exit().transition()
          .duration(duration)
          .attr("cy", function(i) { return x1(d[i]); })
          .style("opacity", 1e-6)
          .remove();

      // Compute the tick format.
      var format = tickFormat || x1.tickFormat(8);

      // Update box ticks.
      var boxTick = g.selectAll("text.box")
          .data(quartileData);

     /* boxTick.enter().append("text")
          .attr("class", "box")
          .attr("dy", ".3em")
          .attr("dx", function(d, i) { return i & 1 ? 6 : -6 })
          .attr("x", function(d, i) { return i & 1 ? width : 0 })
          .attr("y", x0)
          .attr("text-anchor", function(d, i) { return i & 1 ? "start" : "end"; })
          .text(format)
        .transition()
          .duration(duration)
          .attr("y", x1);*/

      boxTick.transition()
          .duration(duration)
          .text(format)
          .attr("y", x1);

      // Update whisker ticks. These are handled separately from the box
      // ticks because they may or may not exist, and we want don't want
      // to join box ticks pre-transition with whisker ticks post-.
      var whiskerTick = g.selectAll("text.whisker")
          .data(whiskerData || []);

      /*whiskerTick.enter().append("text")
          .attr("class", "whisker")
          .attr("dy", ".3em")
          .attr("dx", 6)
          .attr("x", width)
          .attr("y", x0)
          .text(format)
          .style("opacity", 1e-6)
        .transition()
          .duration(duration)
          .attr("y", x1)
          .style("opacity", 1);*/

      whiskerTick.transition()
          .duration(duration)
          .text(format)
          .attr("y", x1)
          .style("opacity", 1);

      whiskerTick.exit().transition()
          .duration(duration)
          .attr("y", x1)
          .style("opacity", 1e-6)
          .remove();
    });
    d3.timer.flush();
  }

  box.width = function(x) {
    if (!arguments.length) return width;
    width = x;
    return box;
  };

  box.height = function(x) {
    if (!arguments.length) return height;
    height = x;
    return box;
  };

  box.tickFormat = function(x) {
    if (!arguments.length) return tickFormat;
    tickFormat = x;
    return box;
  };

  box.duration = function(x) {
    if (!arguments.length) return duration;
    duration = x;
    return box;
  };

  box.domain = function(x) {
    if (!arguments.length) return domain;
    domain = x == null ? x : d3.functor(x);
    return box;
  };

  box.value = function(x) {
    if (!arguments.length) return value;
    value = x;
    return box;
  };

  box.whiskers = function(x) {
    if (!arguments.length) return whiskers;
    whiskers = x;
    return box;
  };

  box.quartiles = function(x) {
    if (!arguments.length) return quartiles;
    quartiles = x;
    return box;
  };

  return box;
};

function boxWhiskers(d) {
  return [0, d.length - 1];
}

function boxQuartiles(d) {
  return [
    d3.quantile(d, .25),
    d3.quantile(d, .5),
    d3.quantile(d, .75)
  ];
}



////////////////////////////////////////////////////////////////////////////////
//// Function for star plot
////////////////////////////////////////////////////////////////////////////////

var RadarChart = {
  draw: function(id, d, options){
  var cfg = {
	 radius: 5,
	 w: 600,
	 h: 600,
	 factor: 1,
	 factorLegend: .85,
	 levels: 3,
	 maxValue: 0,
	 radians: 2 * Math.PI,
	 opacityArea: 0.5,
	 ToRight: 5,
	 TranslateX: 80,
	 TranslateY: 30,
	 ExtraWidthX: 100,
	 ExtraWidthY: 100,
	 color: d3.scale.category10(),
	 x : 0,
	 y : 0
	};

	if('undefined' !== typeof options){
	  for(var i in options){
		if('undefined' !== typeof options[i]){
		  cfg[i] = options[i];
		}
	  }
	}
	cfg.maxValue = Math.max(cfg.maxValue, d3.max(d, function(i){return d3.max(i.map(function(o){return o.value;}))}));
	var allAxis = (d[0].map(function(i, j){return i.axis}));
	var total = allAxis.length;
	var radius = cfg.factor*Math.min(cfg.w/2, cfg.h/2);
	var Format = d3.format('%');
	var g = d3.select(id).select("svg").append("g").attr("transform", "translate("+cfg.x+","+cfg.y+")");     //  transform graph !!!




	var tooltip;

	//Circular segments
	for(var j=0; j<cfg.levels; j++){
	  var levelFactor = cfg.factor*radius*((j+1)/cfg.levels);
	  g.selectAll(".levels")
	   .data(allAxis)
	   .enter()
	   .append("svg:line")
	   .attr("x1", function(d, i){return levelFactor*(1-cfg.factor*Math.sin(i*cfg.radians/total));})
	   .attr("y1", function(d, i){return levelFactor*(1-cfg.factor*Math.cos(i*cfg.radians/total));})
	   .attr("x2", function(d, i){return levelFactor*(1-cfg.factor*Math.sin((i+1)*cfg.radians/total));})
	   .attr("y2", function(d, i){return levelFactor*(1-cfg.factor*Math.cos((i+1)*cfg.radians/total));})
	   .attr("class", "line")
	   .style("stroke", "#414141")
	   .style("stroke-opacity", "0.75")
	   .style("stroke-width", "0.3px")
	   .attr("transform", "translate(" + ((cfg.w/2)-levelFactor) + ", " + (cfg.h/2-levelFactor) + ")");
	}

	//Text indicating at what % each level is
	/*for(var j=0; j<cfg.levels; j++){
	  var levelFactor = cfg.factor*radius*((j+1)/cfg.levels);
	  g.selectAll(".levels")
	   .data([1]) //dummy data
	   .enter()
	   .append("svg:text")
	   .attr("x", function(d){return levelFactor*(1-cfg.factor*Math.sin(0));})
	   .attr("y", function(d){return levelFactor*(1-cfg.factor*Math.cos(0));})
	   .attr("class", "legend")
	   .style("font-family", "sans-serif")
	   .style("font-size", "10px")
	   .attr("transform", "translate(" + (cfg.w/2-levelFactor + cfg.ToRight) + ", " + (cfg.h/2-levelFactor) + ")")
	   .attr("fill", "#737373")
	   .text(Format((j+1)*cfg.maxValue/cfg.levels));
	}*/

	series = 0;

	var axis = g.selectAll(".axis")
			.data(allAxis)
			.enter()
			.append("g")
			.attr("class", "axis");

	axis.append("line")
		.attr("x1", cfg.w/2)
		.attr("y1", cfg.h/2)
		.attr("x2", function(d, i){return cfg.w/2*(1-cfg.factor*Math.sin(i*cfg.radians/total));})
		.attr("y2", function(d, i){return cfg.h/2*(1-cfg.factor*Math.cos(i*cfg.radians/total));})
		.attr("class", "line")
		.style("stroke", "grey")
		.style("stroke-width", "1px");

	/*axis.append("text")
		.attr("class", "legend")
		.text(function(d){return d})
		.style("font-family", "sans-serif")
		.style("font-size", "8px")
		.attr("text-anchor", "middle")
		.attr("dy", "1.5em")
		.attr("transform", function(d, i){return "translate(5, -5)"})
		.attr("x", function(d, i){return cfg.w/2*(1-cfg.factor*Math.sin(i*cfg.radians/total));})
		.attr("y", function(d, i){return cfg.h/2*(1-Math.cos(i*cfg.radians/total))-20*Math.cos(i*cfg.radians/total);});*/


	d.forEach(function(y, x){
	  dataValues = [];
	  g.selectAll(".nodes")
		.data(y, function(j, i){
		  dataValues.push([
			cfg.w/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total)),
			cfg.h/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total))
		  ]);
		});
	  dataValues.push(dataValues[0]);
	  g.selectAll(".area")
					 .data([dataValues])
					 .enter()
					 .append("polygon")
					 .attr("class", "radar-chart-serie"+series)
					 .style("stroke-width", "1px")
					 .style("stroke", "#112E45")
					 .attr("points",function(d) {
						 var str="";
						 for(var pti=0;pti<d.length;pti++){
							 str=str+d[pti][0]+","+d[pti][1]+" ";
						 }

						 return str;
					  })
					 .style("fill", function(j, i){return "#77ADD9"})
					 .style("fill-opacity", cfg.opacityArea)
					 .on('mouseover', function (d){
										z = "polygon."+d3.select(this).attr("class");
										g.selectAll("polygon")
										 .transition(200)
										 .style("fill-opacity", 0.3);
										g.selectAll(z)
										 .transition(200)
										 .style("fill-opacity", 0.2);
									  })
					 .on('mouseout', function(){
										g.selectAll("polygon")
										 .transition(200)
										 .style("fill-opacity", cfg.opacityArea);
					 });
	  series++;
	});
	series=0;


	d.forEach(function(y, x){
	  g.selectAll(".nodes")
		.data(y).enter()
		.append("svg:circle")
		.attr("class", "radar-chart-serie"+series)
		.attr('r', cfg.radius)
		.attr("alt", function(j){return Math.max(j.value, 0)})
		.attr("cx", function(j, i){
		  dataValues.push([
			cfg.w/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total)),
			cfg.h/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total))
		]);
		return cfg.w/2*(1-(Math.max(j.value, 0)/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total));
		})
		.attr("cy", function(j, i){
		  return cfg.h/2*(1-(Math.max(j.value, 0)/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total));
		})
		.attr("data-id", function(j){return j.axis})
		.style("fill", "#112E45").style("fill-opacity", .9)
		.on('mouseover', function (d){
					newX =  parseFloat(d3.select(this).attr('cx')) - 10;
					newY =  parseFloat(d3.select(this).attr('cy')) - 5;

					/*tooltip
						.attr('x', newX)
						.attr('y', newY)
						.text(d.realValue)
						.transition(200)*/

					d3.select(this).style("fill", "#C9E5FC");

					d3.select('#plot-message').text(function () {
						var ch = d.axis+': ' + d.realValue;
						return ch;
					});

					z = "polygon."+d3.select(this).attr("class");
					g.selectAll("polygon")
						.transition(200)
						.style("fill-opacity", 0.3);
					g.selectAll(z)
						.transition(200)
						.style("fill-opacity", 0.2);
				  })
		.on('mouseout', function(){
					/*tooltip
						.transition(200)
						.style('opacity', 0);*/

					d3.select(this).style("fill", "#112E45");

					g.selectAll("polygon")
						.transition(200)
						.style("fill-opacity", cfg.opacityArea);
				  })
		.append("svg:title");

	  series++;
	});
	//Tooltip
	tooltip = g.append('text')
			   .style('opacity', 0)
			   .style('font-family', 'sans-serif')
			   .style('font-size', '13px');
  }
};


////////////////////////////////////////////////////////////////////////////////
//// The widget
////////////////////////////////////////////////////////////////////////////////

HTMLWidgets.widget({

  name: 'aweSOMwidget',

  type: 'output',

  factory: function(el, width, height) {

    return {
           renderValue: function(data) {

console.log("Enter aweSOMwidget\n");

    //var svg = d3.select(el).append("svg").style("width", width).style("height", height);

    document.getElementById("cell-info").style.textAlign = "center";
    document.getElementById("plot-message").style.textAlign = "center";

    //remove the old graph
    document.getElementById("theWidget").innerHTML = "";
    document.getElementById("theWidget").style.paddingBottom = ((cellSize*nbRows) + "px");
    // remove old plot messages
    document.getElementById("cell-info").innerHTML = "Hover over the plot for information.";
    document.getElementById("plot-message").innerHTML = "-";
    //document.getElementById("plot-names").style.margin-top = "50%";

    document.getElementById("plot-names").innerHTML = "-";


    // Import common data
    if(data == null ) {return;}

    //console.log(data); //to evaluate data structure
    var plotType= data.plotType;
    var nbRows= data.gridInfo.nbLines;
    var nbColumns= data.gridInfo.nbColumns;
    var topology= data.gridInfo.topology;
    var saveToPng=data.saveToPng;
    var cellSize=data.sizeInfo;
    var w = cellSize*nbColumns;
    var h = cellSize*nbRows;
    var superclass = data.superclass;
    var superclassColor = data.superclassColor;
  	var cellNames = data.cellNames;
  	var cellPop = data.cellPop;

    // Plot-specific data
    if(plotType.localeCompare("Radar")==0) {
      // Radar
      var parts = data.parts;
      var label = data.label;
      var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
      var radarNormalizedSize = data.radarNormalizedSize;
      var radarRealSize = data.radarRealSize;
      var radarNormalizedValues = data.radarNormalizedValues;
      var radarRealValues = data.radarRealValues;
    } else if(plotType.localeCompare("Camembert")==0) {
      // Pie
      var parts = data.parts;
      var label = data.label;
      var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
      var pieNormalizedSize = data.pieNormalizedSize;
      var pieRealSize = data.pieRealSize;
      var pieNormalizedValues = data.pieNormalizedValues;
      var pieRealValues = data.pieRealValues;
    } else if(plotType.localeCompare("Barplot")==0) {
      // Barplot
      var nbBatons = data.nbBatons;
      var isHist = data.isHist;
      var isCatBarplot = data.isCatBarplot;
	    var label = data.label;
	    var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
	    var batonNormalizedValues = data.batonNormalizedValues;
	    var batonRealValues = data.batonRealValues;
    } else if(plotType.localeCompare("Boxplot")==0) {
      // Boxplot
      var nbBox = data.nbBox;
      var label = data.label;
    	var labelColor = data.labelColor;
      if(!Array.isArray(label)) {label= [label];}
      if(!Array.isArray(labelColor)) {labelColor= [labelColor];}
    	var boxPlotNormalizedValues = data.boxPlotNormalizedValues;
    	var boxPlotRealValues = data.boxPlotRealValues;
    	var boxNormalizedExtremesValues= data.boxNormalizedExtremesValues;
    	var boxRealExtremesValues= data.boxRealExtremesValues;
    } else if(plotType.localeCompare("Color")==0) {
      // Color
      var activate = data.activate;
      var colorNormalizedValues = data.colorNormalizedValues;
    	var colorRealValues = data.colorRealValues;
    	var label = data.label;
    } else if(plotType.localeCompare("Star")==0) {
      // Star
      var nbSommet = data.nbSommet;
      var label = data.label;
      if(!Array.isArray(label)) {label= [label];}
    	var starPlotNormalizedValues = data.starPlotNormalizedValues;
    	var starPlotRealValues = data.starPlotRealValues;
    } else if(plotType.localeCompare("Hitmap")==0) {
      // Hitmap
      var hitmapNormalizedValues = data.hitmapNormalizedValues;
      var hitmapRealValues = data.hitmapRealValues;
    } else if(plotType.localeCompare("Line")==0) {
      // Lines
      var nbPoints = data.nbPoints;
      var label = data.label;
      var lineNormalizedValues = data.lineNormalizedValues;
    	var lineRealValues = data.lineRealValues;
    } else if(plotType.localeCompare("Names")==0) {
      // Wordcloud of names
      var nbWord = data.nbWord;
      var wordClouds = data.wordClouds;
    }

    // Plot download handler
    function downloadCanvas(link, filename) {
      var svg = el.children[0];
      var img = document.getElementById("fromcanvasPlot");

      if(saveToPng){
        svg.toDataURL("image/png", {
          callback: function(data) {
            link.href = data;
            link.download = filename;
          }
        })
      }
      else {
        svg.toDataURL("image/svg+xml", {
          callback: function(data) {
            link.href = data;
            link.download = filename;
          }
        })
      }
    }
    document.getElementById('downloadLink').addEventListener('click', function() {
      if(saveToPng){
        downloadCanvas(this, 'somplot.png');
      }else{
        downloadCanvas(this, 'somplot.svg');
      }
    }, false);

    // Call grid following topology and type
    if(topology.localeCompare('rectangular')==0){
      commonSquareGrid();
    } else if(topology.localeCompare('hexagonal')==0){
      commonHexGrid();
    }

    // Star function
//    if(plotType.localeCompare("Star")==0) {


      function StarChart(id, d, options) {
        var cfg = {
      	 radius: 5,
      	 w: 600,
      	 h: 600,
      	 factor: 1,
      	 factorLegend: .85,
      	 levels: 3,
      	 maxValue: 0,
      	 radians: 2 * Math.PI,
      	 opacityArea: 0.5,
      	 ToRight: 5,
      	 TranslateX: 80,
      	 TranslateY: 30,
      	 ExtraWidthX: 100,
      	 ExtraWidthY: 100,
      	 color: d3.scale.category10(),
      	 x : 0,
      	 y : 0
      	};

      	if('undefined' !== typeof options){
      	  for(var i in options){
        		if('undefined' !== typeof options[i]){
        		  cfg[i] = options[i];
        		}
      	  }
      	}
      	cfg.maxValue = Math.max(cfg.maxValue, d3.max(d, function(i){return d3.max(i.map(function(o){return o.value;}))}));
      	var allAxis = (d[0].map(function(i, j){return i.axis}));
      	var total = allAxis.length;
      	var radius = cfg.factor*Math.min(cfg.w/2, cfg.h/2);
      	var Format = d3.format('%');
      	var g = d3.select(id).select("svg").append("g").attr("transform", "translate("+cfg.x+","+cfg.y+")");
      	var tooltip;

      	//Circular segments
      	for(var j=0; j<cfg.levels; j++){
      	  var levelFactor = cfg.factor*radius*((j+1)/cfg.levels);
      	  g.selectAll(".levels")
      	   .data(allAxis)
      	   .enter()
      	   .append("line")
      	   .attr("x1", function(d, i){return levelFactor*(1-cfg.factor*Math.sin(i*cfg.radians/total));})
      	   .attr("y1", function(d, i){return levelFactor*(1-cfg.factor*Math.cos(i*cfg.radians/total));})
      	   .attr("x2", function(d, i){return levelFactor*(1-cfg.factor*Math.sin((i+1)*cfg.radians/total));})
      	   .attr("y2", function(d, i){return levelFactor*(1-cfg.factor*Math.cos((i+1)*cfg.radians/total));})
      	   .attr("class", "line")
      	   .style("stroke", "#414141")
      	   .style("stroke-opacity", "0.75")
      	   .style("stroke-width", "0.3px")
      	   .attr("transform", "translate(" + ((cfg.w/2)-levelFactor) + ", " + (cfg.h/2-levelFactor) + ")");
      	}

      	series = 0;
      	var axis = g.selectAll(".axis")
      			.data(allAxis)
      			.enter()
      			.append("g")
      			.attr("class", "axis");
      	axis.append("line")
      		.attr("x1", cfg.w/2)
      		.attr("y1", cfg.h/2)
      		.attr("x2", function(d, i){return cfg.w/2*(1-cfg.factor*Math.sin(i*cfg.radians/total));})
      		.attr("y2", function(d, i){return cfg.h/2*(1-cfg.factor*Math.cos(i*cfg.radians/total));})
      		.attr("class", "line")
      		.style("stroke", "grey")
      		.style("stroke-width", "1px");

      	d.forEach(function(y, x){
      	  dataValues = [];
      	  g.selectAll(".nodes")
        		.data(y, function(j, i){
        		  dataValues.push([
        			cfg.w/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total)),
        			cfg.h/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total))
        		  ]);
        		});
      	  dataValues.push(dataValues[0]);

      	  g.selectAll(".area")
      					 .data([dataValues])
      					 .enter()
      					 .append("polygon")
      					 .attr("class", "radar-chart-serie"+series)
      					 .style("stroke-width", "1px")
      					 .style("stroke", "#112E45")
      					 .attr("points",function(d) {
      						 var str="";
      						 for(var pti=0;pti<d.length;pti++){
      							 str=str+d[pti][0]+","+d[pti][1]+" ";
      						 }

      						 return str;
      					  })
      					 .style("fill", function(j, i){return "#77ADD9"})
      					 .style("fill-opacity", cfg.opacityArea)
      					 /*.on('mouseover', function (d){
      										z = "polygon."+d3.select(this).attr("class");
      										g.selectAll("polygon")
      										 .transition(200)
      										 .style("fill-opacity", 0.3);
      										g.selectAll(z)
      										 .transition(200)
      										 .style("fill-opacity", 0.2);
      									  })
      					 .on('mouseout', function(){
      										g.selectAll("polygon")
      										 .transition(200)
      										 .style("fill-opacity", cfg.opacityArea);
      					 })*/;
      	  series++;
      	});
      	series=0;

      	d.forEach(function(y, x){
      	  var circles= g.selectAll(".nodes")
      		.data(y).enter()
      		.append("circle")
      		.attr("class", "radar-chart-serie"+series)
      		.attr('r', cfg.radius)
      		.attr("alt", function(j){return Math.max(j.value, 0)})
      		.attr("cx", function(j, i){
      		  dataValues.push([
      			cfg.w/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total)),
      			cfg.h/2*(1-(parseFloat(Math.max(j.value, 0))/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total))
      		]);
      		return cfg.w/2*(1-(Math.max(j.value, 0)/cfg.maxValue)*cfg.factor*Math.sin(i*cfg.radians/total));
      		})
      		.attr("cy", function(j, i){
      		  return cfg.h/2*(1-(Math.max(j.value, 0)/cfg.maxValue)*cfg.factor*Math.cos(i*cfg.radians/total));
      		})
      		.attr("data-id", function(j){return j.axis})
      		.style("fill", "#112E45").style("fill-opacity", .9)
      		//.on('mouseover', function (d){
      		;
      		circles.append("path").attr("class", function(j, i) {return "tavu"+i;});
      		circles.on('mouseover', function (j, i){
      					newX =  parseFloat(d3.select(this).attr('cx')) - 10;
      					newY =  parseFloat(d3.select(this).attr('cy')) - 5;

      					// Highlight
      					//d3.select(this).style("fill", "#C9E5FC");
      					g.selectAll("path.tavu"+i).style("fill", "#C9E5FC");
      					//g.selectAll("circle.radar-chart-serie"+series).style("fill", "#C9E5FC");

      					d3.select('#plot-message').text(function () {
      						var ch = j.axis+': ' + j.realValue;
      						return ch;
      					});

      					z = "polygon."+d3.select(this).attr("class");
      					g.selectAll("polygon")
      						.transition(200)
      						.style("fill-opacity", 0.3);
      					g.selectAll(z)
      						.transition(200)
      						.style("fill-opacity", 0.2);
      				  })
      		.on('mouseout', function(j, i){
      					//d3.select(this).style("fill", "#112E45");
      					g.selectAll("path.tavu"+i).style("fill", "#112E45");

      					g.selectAll("polygon")
      						.transition(200)
      						.style("fill-opacity", cfg.opacityArea);
      				  })
      		.append("title");

      	  series++;
      	});
      	//Tooltip
      	tooltip = g.append('text')
      			   .style('opacity', 0)
      			   .style('font-family', 'sans-serif')
      			   .style('font-size', '13px');
      }
    //}



    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Square grid function
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    function commonSquareGrid(){
      var svg = d3.select(el).append('svg')
      .attr("style"," display:block; margin:auto; margin-top:30px")
      .attr({width: w, height: h});

      if(plotType.localeCompare("Color")==0) {
        ////////////////////////////////////////////////////////////////////////
        // Square color plot
        ////////////////////////////////////////////////////////////////////////
      	_.times(nbRows, function(n) {
    			var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append("g");

          rows.append('rect')
    			.attr({
    				class: function(d, i) {
    					return 'Square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    				},
    				id: function(d, i) {
    					return 's-' + (n + 1) + (i + 1);
    				},
    				width: cellSize,
    				height: cellSize,
    				x: function(d, i) {
    					return i * cellSize;
    				},
    				y: n * cellSize,
    				fill: function(d, i) {
              return colorNormalizedValues[(n*nbColumns)+i];
    				},
    				stroke: '#FDBB30'
    			});
      		if(activate){ // Superclass numbers on cells for Color plot
      			rows.append("text")
      				.attr("x", function(d, i) { return i * cellSize + cellSize*45/100; })
      				.attr("y", n * cellSize + cellSize*50/100)
      				.text(function(d, i) { return superclass[n*nbColumns+i]; })
      				.attr("font-family", "sans-serif")
      				.attr("font-size", cellSize*20/100)
      				.attr("fill", "#112E45");
      		}
      		rows.on('mouseover', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1 + (n*nbColumns)+i) + ', Superclass ' +
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
      			d3.select('#plot-message').text(function () {
      				return label + ': ' + colorRealValues[(n*nbColumns)+i];
      				});
      			d3.select('#plot-names').text(function () { //for the box below
      				return cellNames[(n*nbColumns)+i];
      				});
        		});
        	rows.on('mouseout', function (d, i) {
        			var el = d3.select(this)
        				.transition()
        				.duration(1000)
        				.style("fill-opacity", 1);
        		});
      			d3.select('#plot-message').text(function () {
      				return '-';
      			});
        	});
      } else if(plotType.localeCompare("Hitmap")==0) {
        //////////////////////////////////////////////////////////////////////
        // Square Hitmap
        //////////////////////////////////////////////////////////////////////
      	_.times(nbRows, function(n) {
    			var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append('rect')
    				.attr({
    					class: function(d, i) {
    						return 'square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    					},
    					id: function(d, i) {
    						return 's-' + (n + 1) + (i + 1);
    					},
    					width: cellSize,
    					height: cellSize,
    					x: function(d, i) {
    						return i * cellSize;
    					},
    					y: n * cellSize,
    					fill: function(d, i) {
    						var indice = superclass[(n*nbColumns)+i];
    						return superclassColor[indice-1];
    					},
    					stroke: '#FDBB30'
    				});
    			var inside = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append('rect')
    				.attr("class", "squareIn")
    				.attr({
    				  class: function(d, i) {
    					return 'row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    				  },
    				  id: function(d, i) {
    					return 's-' + (n + 1) + (i + 1);
    				  },
    				  width: function(d, i) {
    					return hitmapNormalizedValues[(n*nbColumns)+i]*cellSize*0.97;
    				  },
    				  height: function(d, i) {
    					return hitmapNormalizedValues[(n*nbColumns)+i]*cellSize;
    				  },
    				  x: function(d, i) {
    					return (i * cellSize + cellSize/2 - (hitmapNormalizedValues[(n*nbColumns)+i]*cellSize)*0.97/2);
    				  },
    				  y: function(d, i) {
    					return (n * cellSize + cellSize/2 - (hitmapNormalizedValues[(n*nbColumns)+i]*cellSize)*0.97/2);
    				  },
    				  fill: '#112E45',
    				  });

      		rows.on('mouseover', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1+ ((nbRows-n-1)*nbColumns) + i) + ', Superclass ' + //fixed
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
      			d3.select('#plot-names').text(function () {
      				return cellNames[(n*nbColumns)+i];
      				});
        		});

    			rows.on('mouseout', function (d, i) {
    				var el = d3.select(this)
    					.transition()
    					.duration(1000)
    					.style("fill-opacity", 1);
    			});
    		});

      } else if (plotType.localeCompare("Line")==0) {
        //////////////////////////////////////////////////////////////////////
        // Square Lines
        //////////////////////////////////////////////////////////////////////
        _.times(nbRows, function(n) {
    			var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter().append('rect')
    				.attr({
    					class: function(d, i) {
    						return 'square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    					},
    					id: function(d, i) {
    						return 's-' + (n + 1) + (i + 1);
    					},
    					width: cellSize,
    					height: cellSize,
    					x: function(d, i) {
    						return i * cellSize;
    					},
    					y: n * cellSize,
    					fill: function(d, i) {
    						var indice = superclass[(n*nbColumns)+i];
    						return superclassColor[indice-1];
    					},
    					stroke: '#FDBB30'
    				});

    			var points = svg.selectAll('rect' + ' .row-' + (n + 1))
    				.data(d3.range(nbColumns))
    				.enter()
    				.append("path")
    				.attr("class", "ligne")
    				.attr("d", function(d, i) {
    				  if (cellPop[(n*nbColumns)+i] == 0) return null;
    					innerArrayNormalizedValues= lineNormalizedValues[(n*nbColumns)+i];
    					innerArrayRealValues= lineRealValues[(n*nbColumns)+i];

    					var arrayValues = [];
    					for(var j=0; j<nbPoints; j++){
    						arrayValues[j] = [];
    						arrayValues[j].px = cellSize*10/100+j*((cellSize-(cellSize*20/100))/(nbPoints-1));
    						arrayValues[j].py = innerArrayNormalizedValues[j]*cellSize;
    						arrayValues[j].realValue = innerArrayRealValues[j];
    					}

    					var lineFunction = d3.svg.line()
                            .x(function(d) { return d.px+i*cellSize; })
                            .y(function(d) { return -d.py+(n+1)*cellSize; })
                            .interpolate("linear");

    					return lineFunction(arrayValues);
    				})
    				.attr("stroke", function(d) {
    					return "#112E45";
    				})
    				.attr("stroke-width", 1.2)
    				.attr("fill", "none");

    			var focus = points.select("path.ligne")
    				.data(d3.range(nbColumns))
    				.enter()
    				.append("circle")
    				.attr("class", function(d, i) {
    					return "y"+(n*nbColumns+i);
    				})
    				.attr("cx", function(d, i) {
    					var px = cellSize*10/100+i*cellSize;
    					return px;
    				})
    				.attr("cy", function(d, i) {
    					innerArrayNormalizedValues= lineNormalizedValues[(n*nbColumns)+i];
    					var py = (-innerArrayNormalizedValues[0]*cellSize)+(n+1)*cellSize;
    					return py;
    				})
    				//.attr("r", 4) //.attr("r", 4)
            .attr("r", function(d,i){
              if(lineNormalizedValues[(n*nbColumns)+i][i] != null &&  lineNormalizedValues[(n*nbColumns)+i][i] != 0){ //what I edited

                //console.log(lineNormalizedValues[(n*nbColumns)+i][1])
                return 4;
              }
              else{
                return 0;
              }

            })
    				.style("fill", "none")
    				.style("stroke", "#112E45");



    			rows.on('mouseover', function(d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#plot-names').text(function () {
      				return cellNames[(n*nbColumns)+i];
      				});
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1+ ((nbRows-n-1)*nbColumns) + i) + ', Superclass ' + //fixed
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
    			});
    			rows.on('mouseout', function(d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 1);
    			});
    			rows.on('mousemove', function (d, i) { //what do these mousemovde methods provide
    				for(var k=0; k<nbRows; k++){
    					for(var l=0; l<nbColumns; l++){
    						innerArrayNormalizedValues= lineNormalizedValues[(k*nbColumns)+l];
    						innerArrayRealValues= lineRealValues[(k*nbColumns)+l];

    						var arrayValues = [];
    						for(var j=0; j<nbPoints; j++){
    							arrayValues[j] = [];
    							arrayValues[j].px = cellSize*10/100+j*((cellSize-(cellSize*20/100))/(nbPoints-1))+l*cellSize;
    							arrayValues[j].py = -(innerArrayNormalizedValues[j]*cellSize)+(k+1)*cellSize;
    							arrayValues[j].realValue = innerArrayRealValues[j];
    						}

    						var bisectPoints = d3.bisector(function(d) { return d.px; }).left;
    						var x = d3.time.scale().range([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);
    						x.domain([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);

    						var additionalX = (l-i)*cellSize;

    						var x0 = x.invert(d3.mouse(this)[0]+additionalX),
    							p = bisectPoints(arrayValues, x0, 1),
    							d0 = arrayValues[p - 1],
    							d1 = arrayValues[p];
    							if(p<nbColumns){
    								d = x0 - d0.px > d1.px - x0 ? d1 : d0;
    							}
    							d = d0;

    						svg.select("circle.y"+(k*nbColumns+l))
    							.attr("transform", "translate(" + (d.px-cellSize*10/100-l*cellSize)+ "," + (d.py-arrayValues[0].py) + ")");

    						if(l==i && k==n){
    							d3.select('#plot-message').text(function () {
    								var pointValue = d == d0 ? arrayValues[p-1].realValue : arrayValues[p].realValue;
    								var pointLabel = d == d0 ? label[p-1] : label[p];
    								return pointLabel + ": " + pointValue;
    							});
    						}
    					}
    				}
    			});
      	});
      } else {
        //////////////////////////////////////////////////////////////////////
        // Other square plots
        //////////////////////////////////////////////////////////////////////
//merge this together with the previous ones or look for similar elements
        // Loop on rows
        _.times(nbRows, function(n) {
          var rows = svg.selectAll('rect' + ' .row-' + (n + 1))
          .data(d3.range(nbColumns))
    			.enter().append('rect')
    			.attr({
    				class: function(d, i) {
    					return 'square row-' + (n + 1) + ' ' + 'col-' + (i + 1);
    				},
    				id: function(d, i) {
    					return 's-' + (n + 1) + (i + 1);
    				},
    				width: cellSize,
    				height: cellSize,
    				x: function(d, i) {
    					return i * cellSize;
    				},
    				y: n * cellSize,
    				fill: function(d, i) {
    					var indice = superclass[(n*nbColumns)+i];
    					return superclassColor[indice-1];
    				},
    				stroke: '#FDBB30'
    			});

          // Global mouse events
      		rows.on('mouseover', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 0.8);
      			d3.select('#cell-info').text(function () {
      				return 'Cell ' + (1+ ((nbRows-n-1)*nbColumns) + i) + ', Superclass ' + //fixed
      				  superclass[(n*nbColumns)+i] + ', N= ' + cellPop[(n*nbColumns)+i];
      				});
      			d3.select('#plot-names').text(function () {
      				return cellNames[(n*nbColumns)+i];
      				});
        		});
      		rows.on('mouseout', function (d, i) {
      			var el = d3.select(this)
      				.transition()
      				.duration(10)
      				.style("fill-opacity", 1);
        		});

      		var array = d3.range(nbColumns);
          svg.selectAll('rect' + ' .row-' + (n + 1))
          .data(d3.range(nbColumns))
    			.append('g')

          if (plotType.localeCompare("Radar")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Radar
            ///////////////////////////////////////////////////////////////////////
            var array = d3.range(nbColumns);
            var width = cellSize*.5;
            var height = cellSize*.5;
            var radius = Math.min(width, height) / 2;
            for (p = 0; p < array.length; p++) {
              var arc = d3.svg.arc().outerRadius(function function_name(d,i) {
                return d.data.normalizedValue*0.4;});
              var innerArrayNormalizedValues = [];
              innerArrayNormalizedValues= radarNormalizedValues[(n*nbColumns)+p];
              var innerArrayRealValues = [];
              innerArrayRealValues= radarRealValues[(n*nbColumns)+p];

              var arrayValues = [];
              for(var j=0; j<parts; j++){
                arrayValues[j] = [];
                arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
                arrayValues[j].realValue = innerArrayRealValues[j];
              }

              var pie = d3.layout.pie()
              .value(function(d) {  return 100/parts; })
              .sort(null);

              var pieParts = svg.selectAll('rect' + ' .row-' + (n + 1))
              .data(pie(arrayValues))
      				.enter()
      				.append("path")
    					.attr("class", function(d, i) {
      					return "r"+i;
            	})
    					.attr('d', arc)
    					.attr('transform', 'translate(' + (array[p] * cellSize + cellSize/2) +
    						',' + (n * cellSize + cellSize/2) + ')')
    					.attr('fill', function(d, i) {
    						return labelColor[i];
    					});

              // Mouse actions
              pieParts.on('mouseenter', function (d, i) {
                d3.select('#plot-message').text(function () {
                  var innerArrayRealValues = [];
                  var ch=label[i]+": " + d.data.realValue;
                  return ch;
                });
              svg.selectAll("path.r"+i)
    						.attr("stroke","white")
    						.transition()
    						.duration(50)
    						.attr("stroke-width",2);
              });
              pieParts.on('mouseleave', function (d, i) {
                svg.selectAll("path.r"+i)
                  .transition()
      						.duration(50)
    	  					.attr("stroke","none");
                d3.select('#plot-message').text(function () {
                  return "-";
                });
              });
            }
          } else if (plotType.localeCompare("Camembert")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Pie
            ///////////////////////////////////////////////////////////////////////
            var array = d3.range(nbColumns);
            var width = cellSize*.5;
            var height = cellSize*.5;
            var radius = Math.min(width, height) / 2;
            for (p = 0; p < array.length; p++) {
              var arc = d3.svg.arc().outerRadius(pieNormalizedSize[(n*nbColumns)+p]*(cellSize*50/100));
              var innerArrayNormalizedValues = [];
              innerArrayNormalizedValues= pieNormalizedValues[(n*nbColumns)+p];
              var innerArrayRealValues = [];
              innerArrayRealValues= pieRealValues[(n*nbColumns)+p];

    					var arrayValues = [];
    					for(var j=0; j<parts; j++){
    						arrayValues[j] = [];
    						arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
    						arrayValues[j].realValue = innerArrayRealValues[j];
    						arrayValues[j].innerCellPop = cellPop[(n*nbColumns)+p];
    					}

    					var pie = d3.layout.pie()
    						.value(function(d) { return d.normalizedValue; })
    						.sort(null);

    					var pieParts = svg.selectAll('rect' + ' .row-' + (n + 1))
    						.data(pie(arrayValues))
    						.enter()
    						.append('path')
                .attr("class", function(d, i) {
              					return "r"+i;
              	})
    						.attr('d', arc)
    						.attr('transform', 'translate(' + (array[p] * cellSize + cellSize/2) + ',' + (n * cellSize + cellSize/2) + ')')
    						.attr('fill', function(d, i) {
    							return labelColor[i];
    						});

              // Mouse actions
              pieParts.on('mouseenter', function (d, i) {
                d3.select('#plot-message').text(function () {
                  var innerArrayRealValues = [];
                  var ch=label[i]+": n= " + d.data.realValue + " (" +
    						    (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) + "%)";
                  return ch;
                });
              svg.selectAll("path.r"+i)
    						.attr("stroke","white")
    						.transition()
    						.duration(50)
    						.attr("stroke-width",2);
              });
              pieParts.on('mouseleave', function (d, i) {
                svg.selectAll("path.r"+i)
                  .transition()
      						.duration(50)
    	  					.attr("stroke","none");
                d3.select('#plot-message').text(function () {
                  return "-";
                });
              });
            }
          } else if (plotType.localeCompare("Barplot")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Barplot
            ///////////////////////////////////////////////////////////////////////
            var k=0;
        		var array = d3.range(nbColumns);
      			var width = cellSize;
      			var height = cellSize*.6;
            for (var cpt = 0; cpt < array.length; cpt++) {

      				var innerArrayNormalizedValues= batonNormalizedValues[(n*nbColumns)+cpt];
      				var innerArrayRealValues= batonRealValues[(n*nbColumns)+cpt];
      				// Check if single variable
      				if (!Array.isArray(innerArrayNormalizedValues)) {
      				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
      				}
      				if (!Array.isArray(innerArrayRealValues)) {
      				  innerArrayRealValues= [innerArrayRealValues];
      				}

      				var arrayValues = [];
      				for(var j=0; j<nbBatons; j++){
      					arrayValues[j] = [];
      					arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
      					arrayValues[j].realValue = innerArrayRealValues[j];
      					arrayValues[j].innerCellPop = cellPop[(n*nbColumns)+cpt]
      				}

      				var y = d3.scale.linear()
      					.domain([0,nbBatons])
      					.range([0,height])

      				var x = d3.scale.linear()
      					.domain([0,nbBatons])
      					.range([0,width])


      				var xAxis = d3.svg.axis()
      					.scale(x)
      					.orient("bottom");

      				var layout = d3.layout.pie()
      						.value(function(d) { return d.normalizedValue; })
      						.sort(null);

      				var bars = svg.selectAll('rect' + ' .row-' + (n + 1))
      					.data(layout(arrayValues))
      					.enter()
      					.append("rect")
      					.attr("class", function(d, i) {
      						return "r"+i;
      					})
      					.attr("x", function (d, i) {
      						return i*((width-(cellSize*40/100))/nbBatons)+k+(cellSize*20/100);
      					})
      					.attr("y", function (d, i) {
                  return (cellSize - (cellSize*5/100) - (innerArrayNormalizedValues[i]*0.9)*cellSize +n*cellSize); // same here see below
                })
      					.attr("width", function (d) { return ((width-(cellSize*40/100))/nbBatons)-(cellSize*2/100)})
      					.attr("height", function (d, i) {
                          //console.log(innerArrayNormalizedValues[i]*cellSize);
                  return (innerArrayNormalizedValues[i]*0.9)*cellSize; //*0.9 applied to reduce full bar level to not reach the box limit
                })
      					.attr("fill", function(d, i) {
      							return labelColor[i];
      					});
      					k+=cellSize;

      				bars.on('mouseenter', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						var ch=label[i]+": " + d.data.realValue;
      						if (isCatBarplot)
      						  ch= ch + " (" +
      						    (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) +  "%)";
      						return ch;
      					});

      					svg.selectAll("rect.r"+i)
      						.attr("stroke","white")
      						.transition()
      						.duration(50)
      						.attr("stroke-width",2);
      				});

      				bars.on('mouseleave', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						return "-";
      					});
      					svg.selectAll("rect.r"+i).transition()
      						.duration(50)
      						.attr("stroke","none");
      				});
      			}
          } else  if (plotType.localeCompare("Boxplot")==0) {
            //////////////////////////////////////////////////////////////////////
            // Square Boxplot
            ///////////////////////////////////////////////////////////////////////
            var width = (cellSize*80/100)/(nbBox)-(cellSize*10/100),
      				height = (cellSize*70/100);
      			if(nbBox==1){width = (cellSize*20/100);
      						 height = (cellSize*40/100);}
      			var min = Infinity,
      				max = -Infinity;
      			var chart = d3.box()
      				.width(width)
      				.height(height);
      			var array = d3.range(nbColumns);

      			for (p = 0; p < array.length; p++) {
      			  if (cellPop[(n*nbColumns)+p] == 0) {continue;}
      				var innerArrayNormalizedValues = boxPlotNormalizedValues[(n*nbColumns)+p];
      				var innerArrayRealValues = boxPlotRealValues[(n*nbColumns)+p];
      				var innerArrayExtremesNormalizedValues = boxNormalizedExtremesValues[(n*nbColumns)+p];
      				var innerArrayExtremesRealValues = boxRealExtremesValues[(n*nbColumns)+p];

      				var arrayValues = [];
      				var data = [];

      				for(var j=0; j<nbBox; j++){
      					arrayValues[j] = [];
      					arrayValues[j].normalizedValues = innerArrayNormalizedValues[j];
      					arrayValues[j].realValues = innerArrayRealValues[j];

      					var speed = arrayValues[j].normalizedValues;
      					for(l=0; l<5; l++){
      						var e = Math.floor(j),
      						r = Math.floor(l),
      						s = Math.floor(speed[l]*cellSize),
      						d = data[e];

      						if (!d) { d = data[e] = [s];}
      						else { d.push(s);}
      						if (s > max) max = s;
      						if (s < min) min = s;
      					}
      					data[j].realValues = arrayValues[j].realValues;
                data[j].labels_plot = label[j]; // I added this

      				}

      				chart.domain([min, max]);

      				var boxs = svg.selectAll('rect' + ' .row-' + (n + 1))
      					.data(data)
      					.enter()
      					.append("g")
      					.attr("class", "box")
      					.attr("width",  width)
      					.attr("height", height)
      					.attr('transform', function(d, i) {
      						if(nbBox==1){
      							return 'translate(' + (cellSize*40/100+i*((cellSize*40/100)/(nbBox)) + p*cellSize) + ',' + (n * cellSize + cellSize*30/100) + ')';
      						}else{
      							return 'translate(' + ((cellSize*15/100+i*((cellSize*80/100)/(nbBox))) + p*cellSize) + ',' + (n * cellSize + cellSize*15/100) + ')';
      						}
      					})
      					.attr('fill', function(d, i) {
      							return labelColor[i];
      					})
      					.call(chart);

//              // Path for highlight
//    					boxs
//    					  .append("path")
//    					  .attr("class", function(d, i) {
//    					   return "r"+i;
//    					  });


      				for(var j=0; j<nbBox; j++){

      					var arrayExtremesValues = [];
      					var arrNormalizedValues = innerArrayExtremesNormalizedValues[j];
      					var arrRealValues = innerArrayExtremesRealValues[j];

      					for(var l=0; l<arrNormalizedValues.length; l++){
      						arrayExtremesValues[l] = [];
      						arrayExtremesValues[l].normalizedValues = arrNormalizedValues[l];
      						arrayExtremesValues[l].realValues = arrRealValues[l];
      						arrayExtremesValues[l].label = label[j];
      					}

      					var focus = boxs.select("g.box")
      						.data(arrayExtremesValues)
      						.enter()
      						.append("circle")
      						.attr("class", "y")
      						.attr('transform', function(d, i) {
      							if(nbBox==1){
      								return 'translate(' + (cellSize*40/100+j*(cellSize*40/100) + width/2 + p*cellSize) + ',' + (cellSize*0.9 - d.normalizedValues*cellSize*0.9 + n * cellSize) + ')';
      							}else{
      								return 'translate(' + ((cellSize*15/100+j*((cellSize*80/100)/(nbBox)) + width/2) + p*cellSize) + ',' + (cellSize*0.90 - d.normalizedValues*cellSize*0.85 + n * cellSize) + ')';
      							}
      						})
      						.attr("r", cellSize*4/100)
      						.attr('fill', function(d, i) {
      								return labelColor[j];
      						})
      						.style("stroke", "#112E45")
      						.attr("stroke-width",1);

      					// Circles mouse action
      					focus.on('mouseenter', function (d, i) {
      						d3.select('#plot-message').text(function () {
      							var ch=d.label+": " + d.realValues;
      							return ch;
      						});




      						d3.select(this)
      							.transition()
      							.duration(50)
      							.attr("stroke-width",2)
      					});

      					focus.on('mouseleave', function (d, i) {
      						d3.select(this).transition()
      							.duration(50)
      							.attr("stroke-width",1);
      					});
      				}


      				boxs.on('mouseenter', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						var ch=label[i]+ ": Q1= " + d.realValues[1] + " ; Med= " +
      							  d.realValues[2] + " ; Q3= " + d.realValues[3];
      						return ch;
      					});


                var selected = d3.select(this).data()[0].labels_plot;
                //console.log(selected);

      					//svg.selectAll("path.r"+i)
    						d3.selectAll("g.box")
      						.transition()
      						.duration(50)
                  .attr("stroke-width", function(d, i){ //this conditional is probably the right way of doing it
                    if(d.labels_plot == selected) {
                    //console.log(this.labels_plot);
                      return 2}
                    else {return 1}

                  });
      				});
      				boxs.on('mouseleave', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						return "-";
      					});
      					//svg.selectAll("path.r"+i)
    						d3.select(this)
      					  .transition()
      						.duration(50)
      						.attr("stroke-width",1);
      				});
      			}
          } else if(plotType.localeCompare("Star")==0) {
            ////////////////////////////////////////////////////////////////////
            // Square Star
            ////////////////////////////////////////////////////////////////////
  		      var array = d3.range(nbColumns);
        		for (p = 0; p < array.length; p++) {
      				var innerArrayNormalizedValues= starPlotNormalizedValues[(n*nbColumns)+p];
      				var innerArrayRealValues= starPlotRealValues[(n*nbColumns)+p];
      				// Check if single variable
      				if (!Array.isArray(innerArrayNormalizedValues)) {
      				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
      				}
      				if (!Array.isArray(innerArrayRealValues)) {
      				  innerArrayRealValues= [innerArrayRealValues];
      				}

      				var arrayValues = new Array(1);
      				arrayValues[0] = new Array(nbSommet);
      				for(var j=0; j<nbSommet; j++){
      					arrayValues[0][j] = {axis: label[j], value: innerArrayNormalizedValues[j], realValue: innerArrayRealValues[j]};
      				}
      				// Dimension of the graph
      				var w = cellSize*80/100,
      					h = cellSize*80/100;
      				//Options for the Radar chart, other than default
      				var mycfg = {
      					w: w,
      					h: h,
      					levels: 6,
      					ExtraWidthX: cellSize,
      					radius: cellSize*5/100,
      					x: (p * cellSize)+cellSize*10/100,
      					y: (n * cellSize)+cellSize*10/100.
      				}
      				//Call function to draw the Radar chart
      				//Will expect that data is in %'s
      				// RadarChart.draw("#thePlot", arrayValues, mycfg);
      				StarChart("#theWidget", arrayValues, mycfg);

      			}
          } else if(plotType.localeCompare("Names")==0) {
            ////////////////////////////////////////////////////////////////////
            // Square names wordcloud
            ////////////////////////////////////////////////////////////////////
      			function draw(words) {
    					svg.selectAll('rect' + ' .row-' + (n + 1))
    						.data(words)
    						.enter()
    						.append("g")
    						.attr('transform', 'translate(' + (array[p] * cellSize + cellSize/2) + ',' + (n * cellSize + cellSize/2 ) + ')')
    						.append("text")
    						.style("font-size", function(d) { return d.size + "px"; })
    						.style("fill", function(d) { return "#112E45"; })
    						.attr("text-anchor", "middle")
    						.attr("transform", function(d) {
    							return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
    						})
    						.text(function(d) { return d.text; });
    				}

        		for (p = 0; p < array.length; p++) {
      				var innerArrayWordClouds= wordClouds[(n*nbColumns)+p];
      				var nbWordInnerArray = 	nbWord[(n*nbColumns)+p];
      				var arrayValues = [];
      				var wordWithMaxLetters=0;
      				for(var j=0; j<nbWordInnerArray; j++){
      					arrayValues[j] = [];
      					arrayValues[j] = innerArrayWordClouds[j];
      					if(arrayValues[j].length>wordWithMaxLetters){
      						wordWithMaxLetters=arrayValues[j].length;
      					}
      				}

      				d3.layout.cloud().size([cellSize, cellSize]) // => ici l'intervale est tres important car il joue sur l'apparence des mots !
      					.words(arrayValues.map(function(d) {
      						return {text: d, size: (cellSize/wordWithMaxLetters) };
      					}))
      					.rotate(function() { return ~~(Math.random() * 1) * 90; })
      					.fontSize(function(d) { return d.size; })
      					.on("end", draw)
      					.start();
      			}
          }
        });
      }
    }


    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // Hexagonal grid function
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    function commonHexGrid(){
      var margin = {top: 30, right: 20, bottom: 20, left: 50};
      var width = 850;
      var height = 350;
      var widtht = 850;
      var heightt = 350;
      var hexRadius=cellSize/2;
      var hexInRadius;

      //Set the new height and width of the SVG based on the max possible
      width = nbColumns*hexRadius*Math.sqrt(3)+hexRadius;
      height = nbRows*1.5*hexRadius+2.5*hexRadius;
      //Set the hexagon radius
      var hexbin = d3.hexbin().radius(hexRadius);
      //Calculate the center positions of each hexagon
      var points = [];
       //for (var i = 0; i < nbRows; i++) {
      for (var i = nbRows; i > 0; i--) {  //for (var i = 0; i < nbRows; i++) { <-- this is the edit to change the order of class numbering
        for (var j = 0; j < nbColumns; j++) {
          points.push([(hexRadius * j * 1.75)+hexRadius, (hexRadius * i * 1.5) ]); //+hexRadius added to get different shape
        }
      }

      var svg = d3.select(el).append("svg")
			.attr("width", width+hexRadius+5)
			.attr("height", height)
			.attr("style"," display:block; margin:auto; margin-top:30px;")
			.append("g")
			.attr("transform", "translate(" + hexRadius + "," + hexRadius+ ")");


      if (plotType.localeCompare("Color")==0) {
        ////////////////////////////////////////////////////////////////////////
        // Hexagonal Color plot
        ////////////////////////////////////////////////////////////////////////
        var hexa = svg.selectAll(".hexagon")
    			.data(hexbin(points))
    			.enter().append("g");

    		var coordinatesArray = hexbin(points); //what type of function is hexbin()

    		hexa.append("path")
    			.attr("class", "hexagon")
    			.attr("d", function (d) {
    				return "M" + d.x + "," + d.y + hexbin.hexagon();
    			})
    			.attr("stroke", function (d,i) {
    				return "#fff";
    			})
    			.style("fill", function (d,i) {
            return colorNormalizedValues[i];
    			});

    		if(activate){
    			hexa.append("text")
    					.attr("x", function(d, i) { return coordinatesArray[i].x - cellSize*5/100 ; })
    					.attr("y", function(d, i) { return coordinatesArray[i].y + cellSize*5/100 ; })
    					.text(function(d, i) { return superclass[i]; })
    					.attr("font-family", "sans-serif")
    					.attr("font-size", cellSize*20/100)
    					.attr("fill", "#112E45");
    		}

    		//hover effects for the hexagons to change opacity and get the text for the cell
    		hexa.on('mouseover', function (d, i) {
    			var el = d3.select(this)
    				.transition()
    				.duration(10)
    				.style("fill-opacity", 0.8);
    			d3.select('#cell-info').text(function () {
    				return 'Cell ' + parseInt(i+1,10) + ', superclass ' +
              superclass[i] + ', N= ' + cellPop[i];
    			});
    			d3.select('#plot-message').text(function () {
    				return label + ': ' + colorRealValues[i];
    			});
    			d3.select('#plot-names').text(function () {
    				return cellNames[i];
  				});
    		});
    		hexa.on('mouseout', function (d, i) {
    			var el = d3.select(this)
    				.transition()
    				.duration(1000)
    				.style("fill-opacity", 1);
    		});
      } if (plotType.localeCompare("Hitmap")==0) {
        ////////////////////////////////////////////////////////////////////////
        // Hexagonal Hitmap
        ////////////////////////////////////////////////////////////////////////
        var hexa = svg.selectAll(".hexagon")
    			.data(hexbin(points))
    			.enter().append("path")
    			.attr("class", "hexagon")
    			.attr("d", function (d) {
    				return "M" + d.x + "," + d.y + hexbin.hexagon();
    			})
    			.attr("stroke", function (d,i) {
    				return "#fff";
    			})
    			.attr("stroke-width", "1px")
    			.style("fill", function (d,i) {
    				var indice = superclass[i];
    				return superclassColor[indice-1];
    			});

    		var i=0;
    		var inner_hex  = svg.append("g")
    			.selectAll(".hexagon")
    			.data(hexbin(points))
    			.enter()
    			.append("path")
    			.attr("class", "InnerHexagon")
    			.attr("d", function (d) {
    				hexInRadius = (hitmapNormalizedValues[i]*cellSize)/2;
    				i++;
    				hexbint = d3.hexbin().radius(hexInRadius);

    				return "M" + d.x + "," + d.y + hexbint.hexagon();
    			})
    			.style("fill", function (d,i) {
    				return "#112E45";
    			})


        var el_enter;


        inner_hex.on('mouseover', function (d, i) {
          var el_enter_ins = el_enter
           .transition()
           .duration(10)
           .style("fill-opacity", 0.5);

        })


        inner_hex.on('mouseout', function (d, i) {
          var el_enter_ins = el_enter
           .transition()
           .duration(10)
           .style("fill-opacity", 1);

        })


    		hexa.on('mouseover', function (d, i) {



           el_enter = d3.select(this);

           el_enter
    				.transition()
    				.duration(10)
    				.style("fill-opacity", 0.5);



    			d3.select('#cell-info').text(function () {
    				var ch = 'Cell ' + parseInt(i+1,10) + ', Superclass ' +
              superclass[i] + ', N= ' + cellPop[i];
    				return ch;
    			});
    			d3.select('#plot-names').text(function () {
    				return cellNames[i];
  				});
    		});
    		hexa.on('mouseout', function (d, i) {
          //console.log(el_enter);
    			var el = d3.select(this)
    				.transition()
    				.duration(1000)
    				.style("fill-opacity", 1);
    		});






      } else {
        ////////////////////////////////////////////////////////////////////////
        // Other hexagonal plots
        ////////////////////////////////////////////////////////////////////////
        var hexa = svg.selectAll(".hexagon")
    		.data(hexbin(points))
  			.enter().append("path")
  			.attr("class", "hexagon")
  			.attr("d", function (d) {
  				return "M" + d.x + "," + d.y + hexbin.hexagon();
  			})
  			.attr("stroke", function (d,i) {
  				return "#fff";
  			})
  			.attr("stroke-width", "1px")
  			.style("fill", function (d,i) {
  				var indice = superclass[i];
  				return superclassColor[indice-1];
  			});

        // Hexagon cell mouse actions for all plots
        hexa.on('mouseover', function (d, i) {
  				var el = d3.select(this)
  					.transition()
  					.duration(10)
  					.style("fill-opacity", 0.8);

          d3.select('#cell-info').text(function () {
            var ch = 'Cell ' + parseInt(i+1,10) + ', Superclass ' +
              superclass[i] + ', N= ' + cellPop[i];
            return ch;
          });
    			d3.select('#plot-names').text(function () {
    				return cellNames[i];
  				});
        });
        hexa.on('mouseout', function (d, i) {
          var el = d3.select(this)
					.transition()
					.duration(1000)
					.style("fill-opacity", 1);
        });

        var array = d3.range(nbColumns);
        var width = hexRadius;
        var height = hexRadius;
        var radius = Math.min(width, height)/ 2;

        svg.selectAll(".hexagon").data(d3.range(nbColumns)).append('g')
        var coordinatesArray = hexbin(points);

        if (plotType.localeCompare("Radar")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Radar
          //////////////////////////////////////////////////////////////////////////
          var array = d3.range(nbColumns);
          var width = cellSize*.5;
          var height = cellSize*.5;
          var radius = Math.min(width, height) / 2;
          for (var n = 0; n < nbRows*nbColumns; n++) {
            var arc = d3.svg.arc().outerRadius(function function_name(d,i) { return d.data.normalizedValue*0.4;});
            var innerArrayNormalizedValues = [];
            innerArrayNormalizedValues= radarNormalizedValues[n];
            var innerArrayRealValues = [];
            innerArrayRealValues= radarRealValues[n];
            var arrayValues = [];

            for(var j=0; j<parts; j++){
              arrayValues[j] = [];
              arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
              arrayValues[j].realValue = innerArrayRealValues[j];
            }

            var pie = d3.layout.pie()
            .value(function(d) { return 100/parts; })
            .sort(null);

            var pieParts = svg.append("g")
    				.selectAll(".hexagon")
    				.data(pie(arrayValues))
    				.enter()
    				.append('path')
    				.attr("class", function(d, i) {
      					return "r"+i;
          	})
    				.attr('d', arc)
    				.attr('transform', 'translate(' + (coordinatesArray[n].x) +',' + coordinatesArray[n].y + ')')
    				.attr('fill', function(d, i) {
    					return labelColor[i];
    				})

            pieParts.on('mouseenter', function (d, i) {
              d3.select('#plot-message').text(function () {
                var ch = label[i] + ": " + d.data.realValue;
                return ch;
              });
              svg.selectAll("path.r"+i)
                .attr("stroke","white")
                .transition()
                .duration(50)
                .attr("stroke-width",2);
            });
            pieParts.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
              svg.selectAll("path.r"+i)
                .transition()
                .duration(50)
                .attr("stroke","none");
            });
          }
        } else if(plotType.localeCompare("Camembert")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Pie
          //////////////////////////////////////////////////////////////////////////
          var array = d3.range(nbColumns);
          var width = cellSize*.5;
          var height = cellSize*.5;
          var radius = Math.min(width, height) / 2;
          for (var n = 0; n < nbRows*nbColumns; n++) {
    				var arc = d3.svg.arc().outerRadius(pieNormalizedSize[n]*(cellSize*40/100));

    				var innerArrayNormalizedValues = [];
    				innerArrayNormalizedValues= pieNormalizedValues[n];

    				var innerArrayRealValues = [];
    				innerArrayRealValues= pieRealValues[n];

    				var arrayValues = [];
    				for(var j=0; j<parts; j++){
    					arrayValues[j] = [];
    					arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
    					arrayValues[j].realValue = innerArrayRealValues[j];
    					arrayValues[j].innerCellPop = cellPop[n];
    				}

    				var pie = d3.layout.pie()
    					.value(function(d) { return d.normalizedValue; })
    					.sort(null);

    				var pieParts = svg.append("g")
    					.selectAll(".hexagon")
    					.data(pie(arrayValues))
    					.enter()
    					.append('path')
              .attr("class", function(d, i) {
            					return "r"+i;
            	})
    					.attr('d', arc)
    					.attr('transform', 'translate(' + (coordinatesArray[n].x) +',' + coordinatesArray[n].y + ')')
    					.attr('fill', function(d, i) {
    						return labelColor[i];
    					})

            pieParts.on('mouseenter', function (d, i) {
    					d3.select('#plot-message').text(function () {
    						var ch=label[i]+ ": n= " + d.data.realValue + " (" +
    						  (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) + "%)";
    						return ch;
    					});
  						svg.selectAll("path.r"+i)
    						.attr("stroke","white")
    						.transition()
    						.duration(50)
    						.attr("stroke-width",2);
    				});
            pieParts.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
  						svg.selectAll("path.r"+i)
    					  .transition()
    						.duration(50)
    						.attr("stroke","none");
    				});
    			}
        } else if(plotType.localeCompare("Barplot")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Barplot
          //////////////////////////////////////////////////////////////////////////
        	var array = d3.range(nbColumns);
    			var width = cellSize;
    			var height = cellSize*.6;
        	for (var indice = 0; indice < nbRows*nbColumns; indice++) {
            var innerArrayNormalizedValues= batonNormalizedValues[indice];
            var innerArrayRealValues= batonRealValues[indice];
    				// Check if single variable
    				if (!Array.isArray(innerArrayNormalizedValues)) {
    				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
    				}
    				if (!Array.isArray(innerArrayRealValues)) {
    				  innerArrayRealValues= [innerArrayRealValues];
    				}
            var arrayValues = [];
            for(var j=0; j<nbBatons; j++){
      				arrayValues[j] = [];
      				arrayValues[j].normalizedValue = innerArrayNormalizedValues[j]*cellSize;
      				arrayValues[j].realValue = innerArrayRealValues[j];
      				arrayValues[j].innerCellPop = cellPop[indice]
      			}
            var y = d3.scale.linear()
      				.domain([0,nbBatons])
      				.range([0,height])

      			var x = d3.scale.linear()
      				.domain([0,nbBatons])
      				.range([0,width])

      			var layout = d3.layout.pie()
      					.value(function(d) { return d.normalizedValue; })
      					.sort(null);

      			var bars = svg.append("g").selectAll(".hexagon")
      				.data(layout(arrayValues))
      				.enter()
      				.append("rect")
      				.attr("class", function(d, i) {
      					return "r"+i;
      				})
      				.attr("x", function (d, i) {
      					return i*((width-(cellSize*40/100))/nbBatons)+(cellSize*20/100);
      				})
      				.attr("y", function (d, i) { return -innerArrayNormalizedValues[i]*(cellSize*55/100)+cellSize*25/100; })
      				.attr("width", function (d) { return ((width-(cellSize*40/100))/nbBatons)-(cellSize*2/100)})
      				.attr("height", function (d, i) {return innerArrayNormalizedValues[i]*cellSize*55/100; })
      				.attr('transform', 'translate(' + (coordinatesArray[indice].x-cellSize/2) + ',' + (coordinatesArray[indice].y) + ')')
      				.attr("fill", function(d, i) {
      						return labelColor[i];
      				});

      			bars.on('mouseenter', function (d, i) {
      				d3.select('#plot-message').text(function () {
      					var ch=label[i]+": " + d.data.realValue
      					if (isCatBarplot)
      					  ch= ch + " (" +
    						  (100 * d.data.realValue / d.data.innerCellPop).toFixed(1) + "%)";
      					return ch;
      				});
      				svg.selectAll("rect.r"+i)
      					.attr("stroke","white")
      					.transition()
      					.duration(50)
      					.attr("stroke-width",2);
      			});
      			bars.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
      				svg.selectAll("rect.r"+i).transition()
      						.duration(50)
      						.attr("stroke","none");
      			});

      		}
        } else if(plotType.localeCompare("Boxplot")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Boxplot
          //////////////////////////////////////////////////////////////////////////

          var width = (cellSize*80/100)/(nbBox)-(cellSize*10/100),
      			height = (cellSize*50/100);
      		if(nbBox==1){width = (cellSize*20/100);
      					 height = (cellSize*30/100);}
      		var min = Infinity,
      			max = -Infinity;
      		var chart = d3.box()
      			.width(width)
      			.height(height);
      		var array = d3.range(nbColumns);
      		var coordinatesArray = hexbin(points);

      		for (var p = 0; p < nbRows*nbColumns; p++) {
      		  if (cellPop[p] == 0) {continue;}
      			var innerArrayNormalizedValues = boxPlotNormalizedValues[p];
      			var innerArrayRealValues = boxPlotRealValues[p];
      			var innerArrayExtremesNormalizedValues = boxNormalizedExtremesValues[p];
      			var innerArrayExtremesRealValues = boxRealExtremesValues[p];

      			var arrayValues = [];
      			var data = [];
      			for(var j=0; j<nbBox; j++){
      				arrayValues[j] = [];
      				arrayValues[j].normalizedValues = innerArrayNormalizedValues[j];
      				arrayValues[j].realValues = innerArrayRealValues[j];

      				var speed = arrayValues[j].normalizedValues;

      				for(l=0; l<5; l++){
      					var e = Math.floor(j),
      					r = Math.floor(l),
      					s = Math.floor(speed[l]*cellSize),
      					d = data[e];

      					if (!d) { d = data[e] = [s];}
      					else { d.push(s);}
      					if (s > max) max = s;
      					if (s < min) min = s;
      				}
      				data[j].realValues = arrayValues[j].realValues;
              data[j].labels_plot = label[j]; // I added this
      			}

      			chart.domain([min, max]);

      			var boxs = svg.selectAll("hexagon")
      				.data(data)
      				.enter().append("g")
      				.attr("class", "box")
      				.attr("width",  width)
      				.attr("height", height)
      				.attr('transform', function(d, i) {
      					if(nbBox==1){
      						return 'translate(' + (cellSize*40/100+i*(cellSize*30/100) + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y - cellSize*15/100)+ ')';
      					}else{
      						return 'translate(' + ((cellSize*15/100+i*((cellSize*80/100)/(nbBox))) + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y - cellSize*25/100)+ ')';
      					}
      				})
      				.attr('fill', function(d, i) {
      						return labelColor[i];
      				})
      				.call(chart);

      			for(var j=0; j<nbBox; j++){
      				var arrayExtremesValues = [];
      				var arrNormalizedValues = innerArrayExtremesNormalizedValues[j];
      				var arrRealValues = innerArrayExtremesRealValues[j];

      				for(var l=0; l<arrNormalizedValues.length; l++){
      					arrayExtremesValues[l] = [];
      					arrayExtremesValues[l].normalizedValues = arrNormalizedValues[l];
      					arrayExtremesValues[l].realValues = arrRealValues[l]
      					arrayExtremesValues[l].label = label[j];

      				}



      				var focus = boxs.select("g.box")
      					.data(arrayExtremesValues)
      					.enter()
      					.append("circle")
      					.attr("class", "y")
      					.attr('transform', function(d, i) {
      						if(nbBox==1){
      							return 'translate(' + (cellSize*40/100+j*(cellSize*40/100) + width/2 + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y + cellSize*25/100 - d.normalizedValues*cellSize*60/100) + ')';
      						}else{
      							return 'translate(' + ((cellSize*15/100+j*((cellSize*80/100)/(nbBox)) + width/2) + coordinatesArray[p].x -cellSize/2) + ',' + (coordinatesArray[p].y + cellSize*25/100 - d.normalizedValues*cellSize*52/100) + ')';
      						}
      					})
      					.attr("r", cellSize*3/100)
      					.attr('fill', function(d, i) {
      							return labelColor[j];
      					})
      					.style("stroke", "#112E45")
      					.attr("stroke-width",1);

      				focus.on('mouseenter', function (d, i) {
      					d3.select('#plot-message').text(function () {
      						var ch=d.label+": " + d.realValues;
      						return ch;
      					});
      					d3.select(this) //this
      						.transition()
      						.duration(50)
      						.attr("stroke-width",2)
      				});
      				focus.on('mouseleave', function (d, i) {
                d3.select('#plot-message').text(function () {
                  return "-";
                });
      					d3.select(this).transition()
      						.duration(50)
      						.attr("stroke-width",1);
      				});
      			}

      			boxs.on('mouseenter', function (d, i) {
      				d3.select('#plot-message').text(function () {
    						var ch=label[i]+ ": Q1= " + d.realValues[1] + " ; Med= " +
    							  d.realValues[2] + " ; Q3= " + d.realValues[3];
      					return ch;
      				});
              var selected = d3.select(this).data()[0].labels_plot; // <- this gets me the variable of the hovered boxplot
              //var selected = d3.select("g.box").data();
              //console.log(selected);
              //console.log(selected[0].labels_plot);

      				d3.selectAll("g.box") // this is where it needs to be edited for the hex version!!!!
      					.transition()
      					.duration(50)
      					.attr("stroke-width", function(d, i){ //this conditional is probably the right way of doing it
                  if(d.labels_plot == selected) {
                  //console.log(this.labels_plot);
                    return 4}
                  else {return 1}

                });
      			});
      			boxs.on('mouseleave', function (d, i) {
              d3.select('#plot-message').text(function () {
                return "-";
              });
      				d3.selectAll("g.box").transition()
      					.duration(50)
      					.attr("stroke-width",1);
      			});
      		}
        } else if(plotType.localeCompare("Star")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Star
          //////////////////////////////////////////////////////////////////////////

        	for (p = 0; p < nbRows*nbColumns; p++) {
      			var innerArrayNormalizedValues= starPlotNormalizedValues[p];
      			var innerArrayRealValues= starPlotRealValues[p];
    				// Check if single variable
    				if (!Array.isArray(innerArrayNormalizedValues)) {
    				  innerArrayNormalizedValues= [innerArrayNormalizedValues];
    				}
    				if (!Array.isArray(innerArrayRealValues)) {
    				  innerArrayRealValues= [innerArrayRealValues];
    				}

      			var arrayValues = new Array(1);
      			arrayValues[0] = new Array(nbSommet);
      			for(var j=0; j<nbSommet; j++){
      				arrayValues[0][j] = {axis: label[j], value: innerArrayNormalizedValues[j], realValue: innerArrayRealValues[j]};
      			}

      			// Dimension of the graph
      			var w = cellSize*70/100,
      				h = cellSize*70/100;

      			//Options for the Radar chart, other than default
      			var mycfg = {
      				w: w,
      				h: h,
      				levels: 6,
      				ExtraWidthX: cellSize,
      				radius: cellSize*5/100,
      				x: coordinatesArray[p].x+cellSize*15/100,
      				y: coordinatesArray[p].y+cellSize*15/100
      			}

      			//Call function to draw the Radar chart
      			//Will expect that data is in %'s
      			//RadarChart.draw("#thePlot", arrayValues, mycfg);
      			StarChart("#theWidget", arrayValues, mycfg);

      		}
        } else if(plotType.localeCompare("Line")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Line
          //////////////////////////////////////////////////////////////////////////
        	for (var indice = 0; indice < nbRows*nbColumns; indice++) {
        	  if (cellPop[indice] == 0) { continue; }
      			innerArrayNormalizedValues= lineNormalizedValues[indice];
      			innerArrayRealValues= lineRealValues[indice];
      			var arrayValues = [];
      			for(var j=0; j<nbPoints; j++){
      				arrayValues[j] = [];
      				arrayValues[j].px = cellSize*20/100+j*((cellSize-(cellSize*40/100))/(nbPoints-1));
      				arrayValues[j].py = innerArrayNormalizedValues[j]*cellSize*0.5;
      				arrayValues[j].realValue = innerArrayRealValues[j];
      			}
      			var points = svg.append("g").selectAll(".hexagon")
      				.data(d3.range(1))
      				.enter()
      				.append("path")
      				.attr("class", "ligne")
      				.attr("d", function(d, i) {
      					var lineFunction = d3.svg.line()
      						.x(function(d) { return d.px; })
      						.y(function(d) { return -d.py;})
      						.interpolate("linear");
      					return lineFunction(arrayValues);
      				})
      				.attr('transform', 'translate(' + (coordinatesArray[indice].x-cellSize/2) + ',' + (coordinatesArray[indice].y+cellSize*25/100) + ')')
      				.attr("stroke", function(d) {
      					return "#112E45";
      				})
      				.attr("stroke-width", 1.2)
      				.attr("fill", "none");

      			var focus = points.select("path.ligne")
      				.data(d3.range(1))
      				.enter()
      				.append("circle")
      				.attr("class", function(d, i) {
      					return "y"+(indice);
      				})
      				.attr("cx", function(d, i) {
      					var px = cellSize*20/100+i*cellSize;
      					return px;
      				})
      				.attr("cy", function(d, i) {
      					innerArrayNormalizedValues= lineNormalizedValues[indice];
      					var py = (-innerArrayNormalizedValues[0]*cellSize)*0.5;
      					return py;
      				})
      				.attr('transform', 'translate(' + (coordinatesArray[indice].x-cellSize/2) + ',' + (coordinatesArray[indice].y+cellSize*25/100) + ')')
      				.attr("r", 4)
      				.style("fill", "none")
      				.style("stroke", "#112E45");

      			hexa.on('mousemove', function (d, i) { //responsible for the information
      				for(var k=0; k<nbRows; k++){
      					for(var l=0; l<nbColumns; l++){
      						var indice = (parseInt((i/nbRows),10)*nbColumns)+ parseInt((i%nbRows),10);
      						innerArrayNormalizedValues= lineNormalizedValues[(k*nbColumns)+l];
      						innerArrayRealValues= lineRealValues[(k*nbColumns)+l];

      						var arrayValues = [];
      						for(var j=0; j<nbPoints; j++){
      							arrayValues[j] = [];
      							arrayValues[j].px = cellSize*20/100+j*((cellSize-(cellSize*40/100))/(nbPoints-1))+coordinatesArray[(k*nbColumns)+l].x-cellSize/2;
      							arrayValues[j].py = -(innerArrayNormalizedValues[j]*cellSize*0.5)+coordinatesArray[(k*nbColumns)+l].y+cellSize*25/100;
      							arrayValues[j].realValue = innerArrayRealValues[j];
      						}

      						var bisectPoints = d3.bisector(function(d) { return d.px; }).left;
      						var x = d3.time.scale().range([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);
      						x.domain([arrayValues[0].px, arrayValues[arrayValues.length - 1].px]);

      						var additionalX = coordinatesArray[(k*nbColumns)+l].x - coordinatesArray[indice].x;

      						var x0 = x.invert(d3.mouse(this)[0] + additionalX),
      							p = bisectPoints(arrayValues, x0, 1),
      							d0 = arrayValues[p - 1],
      							d1 = arrayValues[p];
      							if(p<nbColumns){
      								d = x0 - d0.px > d1.px - x0 ? d1 : d0;
      							}
      							d = d0;

      						svg.select("circle.y"+((k*nbColumns)+l))
      							.attr("transform", "translate(" + (d.px-cellSize*20/100) + "," + (d.py-arrayValues[0].py+(cellSize-cellSize*75/100)+k*cellSize*75/100) + ")");

      						if(l==parseInt((i%nbRows),10) && k==parseInt((i/nbRows),10)){
      							d3.select('#plot-message').text(function () {
      								var pointValue = d == d0 ? arrayValues[p-1].realValue : arrayValues[p].realValue;
      								var pointLabel = d == d0 ? label[p-1] : label[p];
      								return pointLabel + ': ' + pointValue;
      							});
      						}
      					}
      				}
      			});
      		}
        } else if (plotType.localeCompare("Names")==0) {
          //////////////////////////////////////////////////////////////////////////
          // Hexagonal Wordcloud of Names
          //////////////////////////////////////////////////////////////////////////
      		function draw(words) {
    				svg.append("g").selectAll(".hexagon")
    					.data(words)
    					.enter()
    					.append("g")
    					.attr('transform', 'translate(' + (coordinatesArray[p].x) + ',' + (coordinatesArray[p].y) + ')')
    					.append("text")
    					.style("font-size", function(d) { return d.size + "px"; })
    					.style("fill", function(d) { return "#112E45"; })
    					.attr("text-anchor", "middle")
    					.attr("transform", function(d) {
    						return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
    					})
    					.text(function(d) { return d.text; });
    			}
        	for (p = 0; p < nbRows*nbColumns; p++) {

      			var innerArrayWordClouds= wordClouds[p];
      			var nbWordInnerArray = 	nbWord[p];

      			var arrayValues = [];
      			var wordWithMaxLetters=0;
      			for(var j=0; j<nbWordInnerArray; j++){
      				arrayValues[j] = [];
      				arrayValues[j] = innerArrayWordClouds[j];
      				if(arrayValues[j].length>wordWithMaxLetters){
      					wordWithMaxLetters=arrayValues[j].length;
      				}
      			}

            // => ici l'intervalle est tres important car il joue sur l'apparence des mots !
      			d3.layout.cloud().size([cellSize*0.6, cellSize*0.6])
      				.words(arrayValues.map(function(d) {
      					return {text: d, size: (cellSize/wordWithMaxLetters)*0.7 };
      				}))
      				.rotate(function() { return ~~(Math.random() * 1) * 90; })
      				.fontSize(function(d) { return d.size; })
      				.on("end", draw)
      				.start();

      		}
        }
      }
    }

           }
    };
  }
});
