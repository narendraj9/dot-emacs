;;; elfeed-config.el --- My elfeed config            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi(require 'cl-lib) <narendraj9@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'elfeed)
(require 'youtube-dl)

(setq-default elfeed-search-filter "-junk @1-week-ago +unread")

;; More keybindings
(define-key elfeed-search-mode-map (kbd "j") #'next-line)
(define-key elfeed-search-mode-map (kbd "k") #'previous-line)
(define-key elfeed-show-mode-map (kbd "j") #'next-line)
(define-key elfeed-show-mode-map (kbd "k") #'previous-line)

(setq elfeed-feeds
      '(("https://googleprojectzero.blogspot.com/feeds/posts/default" blog security)
        ("https://peteris.rocks/rss.xml" blog dev systems)
        ("https://overreacted.io/rss.xml" blog dev ui)
        ("http://clojure-goes-fast.com/blog/atom.xml" blog dev clojure)
        ("https://www.bennettnotes.com/index.xml" blog dev)
        ("https://hackattic.com/changelog.rss" challenges dev)
        ("http://pragmaticemacs.com/feed/" blog emacs)
        ("https://www.tedinski.com/feed.xml" blog dev design)
        ("https://www.microsoft.com/en-us/research/feed/" blog dev research)
        ("https://vvvvalvalval.github.io/feed.xml" blog dev clojure)
        ("https://www.pvk.ca/atom.xml" blog dev lisp)
        ("https://kubernetespodcast.com/feeds/audio.xml" blog dev k8s)
        ("http://insideclojure.org/archive/" blog dev clojure)
        ("https://www.joelonsoftware.com/feed/" blog dev)
        ("https://themonadreader.wordpress.com/feed/" blog haskell dev)
        ("https://8thlight.com/blog/feed/rss.xml" blog dev)
        ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml" emacs blog)
        ("http://blog.ioactive.com/feeds/posts/default" blog security)
        ("http://blog.plover.com/index.atom" blog dev)
        ("http://blog.reverberate.org/feeds/posts/default" dev blog)
        ("http://blog.twonegatives.com/rss" blog) ; Two negatives
        ("http://cachestocaches.com/feed/" emacs blog)
        ("http://cestlaz.github.io/rss.xml" emacs blog)
        ("http://emacshorrors.com/feed.atom" blog emacs)
        ("http://feeds.exploringbinary.com/ExploringBinary" blog dev)
        ("http://feeds.wnyc.org/radiolab" audio)
        ("http://firefly.nu/feeds/all.atom.xml" blog dev)
        ("http://gottwurfelt.wordpress.com/feed/" blog math)
        ("http://irreal.org/blog/?feed=rss2" blog)
        ("http://mortoray.com/feed/" blog dev)
        ("http://nullprogram.com/feed/" blog dev nullprogram)
        ("http://planet.emacsen.org/atom.xml" emacs planet)
        ("http://possiblywrong.wordpress.com/feed/" blog math puzzle)
        ("http://slatestarcodex.com/feed/" blog philosophy)
        ("http://steve-yegge.blogspot.com/atom.xml" blog dev)
        ("http://use-the-index-luke.com/blog/feed" blog dev databases)
        ("http://what-if.xkcd.com/feed.atom" blog)
        ("http://www.aaronsw.com/2002/feeds/pgessays.rss" blog) ; Paul Graham's essays
        ("http://www.allthingsdistributed.com/atom.xml" distributed)
        ("http://www.howstuffworks.com/podcasts/stuff-you-should-know.rss" audio)
        ("http://www.mazelog.com/rss" math puzzle)
        ("http://www.reddit.com/r/dailyprogrammer/.rss" subreddit)
        ("http://www.tedunangst.com/flak/rss" dev blog)
        ("https://apfelmus.nfshost.com/rss.xml" blog haskell)
        ("https://blogs.msdn.microsoft.com/oldnewthing/feed" blog dev)
        ("https://github.com/blog/all.atom" blog dev product)
        ("https://martinfowler.com/feed.atom" blog design)
        ("https://nickdesaulniers.github.io/atom.xml" blog dev)
        ("https://vicarie.in/archive.xml")
        ("https://www.debian.org/News/news" debian list)
        ("https://www.masteringemacs.org/feed" blog emacs)
        ("https://www.schneier.com/blog/atom.xml" blog security)
        ("https://www.schneier.com/blog/atom.xml" security)
        ("https://www.snellman.net/blog/rss-index.xml" dev networking)
        ;; ("https://news.ycombinator.com/rss" hn)
        ;; ("http://feeds.feedburner.com/Explosm" comic)
        ;; ("http://feeds.feedburner.com/InvisibleBread" comic)
        ;; ("http://feeds.feedburner.com/LoadingArtist" comic)
        ;; ("http://feeds.feedburner.com/Pidjin" comic)
        ;; ("http://feeds.feedburner.com/lefthandedtoons/awesome" comic)
        ;; ("http://nedroid.com/feed/" comic)
        ;; ("http://piecomic.tumblr.com/rss" comic)
        ;; ("http://www.businesscat.happyjar.com/feed/" comic)
        ;; ("http://www.exocomics.com/feed" comic)
        ;; ("http://www.extrafabulouscomics.com/1/feed" comic)
        ;; ("http://www.goneintorapture.com/rss" comic)
        ;; ("http://www.mrlovenstein.com/rss.xml" comic)
        ;; ("http://www.optipess.com/feed/" comic)
        ;; ("http://www.safelyendangered.com/feed/" comic)
        ;; ("http://www.reddit.com/user/JimKB/submitted.rss" comic)
        ;; ("http://www.smbc-comics.com/rss.php" comic)
        ;; ("http://www.thingsinsquares.com/feed/" comic)
        ;; ("http://www.whompcomic.com/rss.php" comic)
        ;; ("http://xkcd.com/atom.xml" comic)
        ;; ("1veritasium" youtube)
        ;; ("UCO8DQrSp5yEP937qNqTooOw" youtube)      ; Strange Parts
        ;; ("UCQvdU25Eqk3YS9-QnILhKKQ" youtube)
        ;; ("UCXNxwOuuR7LT-SkEfOJiwgA" youtube)      ; Long Plays
        ;; ("UCYO_jab_esuFRV4b17AJtAw" youtube)      ; 3Blue1Brown
        ;; ("UCsXVk37bltHxD1rDPwtNM8Q" youtube)))    ; Kurzgesagt – In a
        ;; ("UCwRqWnW5ZkVaP_lZF7caZ-g" youtube)      ; Retro Game Mechanics
        ;; ("adric22" youtube)                       ; The 8-bit Guy
        ;; ("http://accidental-art.tumblr.com/rss" image math)
        ;; ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
        ;; ("http://backtracks.fm/ycombinator/ycombinator/feed" blog)
        ;; ("http://bit-player.org/feed" blog math)
        ;; ("http://blog.carlosgaldino.com/atom.xml" blog dev)
        ;; ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
        ;; ("http://chainsawsuit.com/feed/" comic)
        ;; ("http://deep-dark-fears.tumblr.com/rss" comic)
        ;; ("http://dvdp.tumblr.com/rss" image)
        ;; ("http://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
        ;; ("http://english.bouletcorp.com/feed/" comic)
        ;; ("http://feeds.feedburner.com/Buttersafe" comic)
        ;; ("http://feeds.feedburner.com/CatVersusHuman" comic)
        ;; ("http://feeds.feedburner.com/channelATE" comic)
        ;; ("http://simblob.blogspot.com/feeds/posts/default" blog dev)
        ;; ("http://www.bitercomics.com/feed/" comic)
        ;; ("http://www.commitstrip.com/en/feed/" comic dev)
        ;; ("https://blog.coinbase.com/rss/" product bitcoin)
        ;; ("https://utcc.utoronto.ca/~cks/space/blog/?atom" blog dev)
        ;; ("https://www.blogger.com/feeds/19727420/posts/default" blog)
        ;; ("https://www.debian.org/security/dsa" debian list security important)
        ;; ("https://www.digitalocean.com/blog/feed" blog product)
        ;; ("whoisjimmy" youtube)                    ; How Ridiculous
        ;; Nutshell
        ))

;; Strike-through all read articles
(set-face-attribute 'elfeed-search-title-face nil :strike-through t)
(set-face-attribute 'elfeed-search-unread-title-face nil :strike-through nil)

;; With 16 connections, my Emacs slows down a bit.
(elfeed-set-max-connections 8)

(provide 'elfeed-config)
;;; elfeed-config.el ends here
