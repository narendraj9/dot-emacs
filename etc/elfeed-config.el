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
      '(("https://vlaaad.github.io/feed.xml")
        ("http://www.natpryce.com/news.feed")
        ("https://feeds.feedburner.com/martinkl")
        ("https://zettelkasten.de/feed.atom")
        ("https://googleprojectzero.blogspot.com/feeds/posts/default")
        ("https://peteris.rocks/rss.xml")
        ("https://overreacted.io/rss.xml")
        ("http://clojure-goes-fast.com/blog/atom.xml")
        ("https://www.bennettnotes.com/index.xml")
        ("https://hackattic.com/changelog.rss")
        ("http://pragmaticemacs.com/feed/")
        ("https://www.tedinski.com/feed.xml")
        ("https://www.microsoft.com/en-us/research/feed/")
        ("https://vvvvalvalval.github.io/feed.xml")
        ("https://www.pvk.ca/atom.xml")
        ("https://kubernetespodcast.com/feeds/audio.xml")
        ("http://insideclojure.org/archive/")
        ("https://www.joelonsoftware.com/feed/")
        ("https://themonadreader.wordpress.com/feed/")
        ("https://8thlight.com/blog/feed/rss.xml")
        ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml")
        ("http://blog.ioactive.com/feeds/posts/default")
        ("http://blog.plover.com/index.atom")
        ("http://blog.reverberate.org/feeds/posts/default")
        ("http://blog.twonegatives.com/rss") ; Two negatives
        ("http://cachestocaches.com/feed/")
        ("http://cestlaz.github.io/rss.xml")
        ("http://emacshorrors.com/feed.atom")
        ("http://feeds.exploringbinary.com/ExploringBinary")
        ("http://feeds.wnyc.org/radiolab")
        ("http://firefly.nu/feeds/all.atom.xml")
        ("http://gottwurfelt.wordpress.com/feed/")
        ("http://irreal.org/blog/?feed=rss2")
        ("http://mortoray.com/feed/")
        ("http://nullprogram.com/feed/")
        ("http://planet.emacsen.org/atom.xml")
        ("http://possiblywrong.wordpress.com/feed/")
        ("http://slatestarcodex.com/feed/")
        ("http://steve-yegge.blogspot.com/atom.xml")
        ("http://use-the-index-luke.com/blog/feed")
        ("http://what-if.xkcd.com/feed.atom")
        ("http://www.aaronsw.com/2002/feeds/pgessays.rss") ; Paul Graham's essays
        ("http://www.allthingsdistributed.com/atom.xml")
        ("http://www.howstuffworks.com/podcasts/stuff-you-should-know.rss")
        ("http://www.mazelog.com/rss")
        ("http://www.reddit.com/r/dailyprogrammer/.rss")
        ("http://www.tedunangst.com/flak/rss")
        ("https://apfelmus.nfshost.com/rss.xml")
        ("https://blogs.msdn.microsoft.com/oldnewthing/feed")
        ("https://github.com/blog/all.atom")
        ("https://martinfowler.com/feed.atom")
        ("https://nickdesaulniers.github.io/atom.xml")
        ("https://vicarie.in/archive.xml")
        ("https://www.debian.org/News/news")
        ("https://www.masteringemacs.org/feed")
        ("https://www.schneier.com/blog/atom.xml")
        ("https://www.schneier.com/blog/atom.xml")
        ("https://www.snellman.net/blog/rss-index.xml")
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
