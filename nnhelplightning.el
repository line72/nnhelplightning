;;; nnhelplightning.el --- Helplightning GNUS backend -*- lexical-binding: t; -*-

(require 'gnus)
(require 'nnheader)
(require 'request)
(require 'seq)

(gnus-declare-backend "nnhelplightning" 'post)

(defvar nnhelplightning-version "1.0.2")

;; (defvar nnhelplightning-instance-url nil)
;; (defvar nnhelplightning-api-key nil)
;; (defvar nnhelplightning-pat nil)

;; environments
(defvar nnhelplightning-environments
      '((us . ((instance-url . "https://api.helplightning.net")
               (api-key . "cuvqrnf3z2z1ngtvtxfonhdrues5dz09")))
        (eu . ((instance-url . "https://api.eu1.helplightning.net")
               (api-key . "tmr6chpcbvbiwgtsys9isdjrn2txut09")))
        (dev . ((instance-url . "https://api.dev.helplightning.net")
               (api-key . "eejxvctmn0hkbm1unzrhyk5nejnhzz09")))))

(defvar nnhelplightning-instances (make-hash-table :test 'equal))


;; (defvar nnhelplightning-token nil "Our authentication token")
;; (defvar nnhelplightning-expiration nil "The expiration time of the token")
;; (defvar nnhelplightning-group-map (make-hash-table :test 'equal) "Mapping of unique names to Helplightnig Workboxes")
;; (defvar nnhelplightning-last-update nil "The last time we updated")

;; (defvar nnhelplightning-status-string "")

;;
;; gnus API
;;
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Required-Back-End-Functions.html#Required-Back-End-Functions-1
;;

(defun nnhelplightning-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve headers for ARTICLES in GROUP using the helplightning backend.

  articles is either a range of article numbers or a list of
  Message-IDs. Current back ends do not fully support either—only
  sequences (lists) of article numbers, and most back ends do not
  support retrieval of Message-IDs. But they should try for both.

  The result data should either be HEADs or NOV lines, and the
  result value should either be headers or nov to reflect
  this. This might later be expanded to various, which will be a
  mixture of HEADs and NOV lines, but this is currently not
  supported by Gnus.

  If fetch-old is non-nil it says to try fetching “extra headers”,
  in some meaning of the word. This is generally done by
  fetching (at most) fetch-old extra headers less than the smallest
  article number in articles, and filling the gaps as well. The
  presence of this parameter can be ignored if the back end finds
  it cumbersome to follow the request. If this is non-nil and not a
  number, do maximum fetches.
  "
  (message "nnhelplightning-retrieve-headers %s" server)
  (if-let ((workbox (gethash group (get-instance-group-map server))))
      (let* ((new-articles (fetch-all-workbox-articles server (gethash 'id workbox)))
            (ordered-articles (sort new-articles (lambda (a b) (< (alist-get 'id a) (alist-get 'id b))))))
        (with-current-buffer nntp-server-buffer
          (erase-buffer)
          (dolist (article ordered-articles)
            (let* ((article-id (alist-get 'id article))
                   (type (alist-get 'type article))
                   (metadata (parse-json-body (alist-get 'metadata article)))
                   (message-body (parse-message type metadata))
                   (user (alist-get 'user article))
                   (from (alist-get 'name user))
                   (iso-date (alist-get 'sent_at article))
                   (date (iso8601-to-nov-date iso-date))
                   (message-id (alist-get 'uuid article))
                   (references nil))
              (when (is-supported-type type)
                (let ((size (length message-body))
                      (lines (length (split-string message-body "\n"))))

                  (insert (format "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d\n"
                                  article-id
                                  date ;; use date as the "subject"
                                  from
                                  date
                                  message-id
                                  (or references "")
                                  size
                                  lines)))))))))

  'nov) ;; Indicates successful retrieval

(defun nnhelplightning-open-server (server &optional defs)
  "Open a connection to the helplightning backend server.

  server is here the virtual server name. definitions is a list
  of (VARIABLE VALUE) pairs that define this virtual server.

  If the server can’t be opened, no error should be signaled. The
  back end may then choose to refuse further attempts at connecting
  to this server. In fact, it should do so.

  If the server is opened already, this function should return a
  non-nil value. There should be no data returned.
  "
  (message "nnhelplightning-open-server: Connecting to helplightning server %s..." server)

  ;; look at the defs and pull out the environment
  ;; make sure it is valid, then set everything up
  (let* ((environment (alist-get 'environment defs))
         (pat (alist-get 'pat defs))
         (env (assoc environment nnhelplightning-environments)))
    (if env
        (let* ((env-info (cdr env))
               (url (alist-get 'instance-url env-info))
               (api-key (alist-get 'api-key env-info))
               (instance (or (get-instance server) (create-instance server))))
          (message "setting up the instance")
          (puthash server instance nnhelplightning-instances)

          (set-instance-url server url)
          (set-instance-api-key server api-key)
          (set-instance-pat server pat)
          
          (connect-server server))
      (error "Error: Environment '%s' not found!" environment))))

(defun nnhelplightning-close-server (&optional server)
  "Close the connection to the helplightning backend server.

  Close connection to server and free all resources connected to
  it. Return nil if the server couldn’t be closed for some
  reason.

  There should be no data returned.
  "
  (message "nnhelplightning-close-server: Disconnecting from helplightning server...")
  (remhash server nnhelplightning-instances)
  t) ;; Return t to indicate success

(defun nnhelplightning-request-close ()
  "Close connection to all servers and free all resources that the
  back end have reserved. All buffers that have been created by
  that back end should be killed. (Not the nntp-server-buffer,
  though.) This function is generally only called when Gnus is
  shutting down.

  There should be no data returned.
  "
  (message "nnhelplightning-request-close")
  t)

(defun nnhelplightning-server-opened (&optional server)
  "Check if the custom backend server is opened.

  If server is the current virtual server, and the connection to
  the physical server is alive, then this function should return
  a non-nil value. This function should under no circumstances
  attempt to reconnect to a server we have lost connection to.

  There should be no data returned.
  "
  (message "nnhelplightning-server-opened: Checking if server is opened: %s" server)

  ;; Check if we have a token AND if it hasn't 
  ;;  expired yet
  (if (and (get-instance-token server)
           (not (is-expired (get-instance-expiration server))))
      t nil))

(defun nnhelplightning-status-message (&optional server)
  "This function should return the last error message from server.

  There should be no data returned.
  "
  (message "nnhelplightning-status-message %s" server)
  (get-instance-status-string server))

(defun nnhelplightning-request-article (article &optional group server to-buffer)
  "Fetch the content of ARTICLE in GROUP from the nnhelplightning backend.
  ARTICLE is the article number as a string.
  GROUP is the group name.
  SERVER is the server name (optional).
  TO-BUFFER, if non-nil, specifies a buffer to write the content into.

  The result data from this function should be the article
  specified by article. This might either be a Message-ID or a
  number. It is optional whether to implement retrieval by
  Message-ID, but it would be nice if that were possible.

  If to-buffer is non-nil, the result data should be returned in
  this buffer instead of the normal data buffer. This is to make
  it possible to avoid copying large amounts of data from one
  buffer to another, while Gnus mainly requests articles to be
  inserted directly into its article buffer.

  If it is at all possible, this function should return a cons
  cell where the car is the group name the article was fetched
  from, and the cdr is the article number. This will enable Gnus
  to find out what the real group and article numbers are when
  fetching articles by Message-ID. If this isn’t possible, t
  should be returned on successful article retrieval.
  "
  (message "nnhelplightning-request-article: Fetching article %s (type: %s) from group %s on server %s"
           article (type-of article) group server)
  ;; Ensure ARTICLE is treated as a string, but convert to a number for alist lookup
  (if (gethash group (get-instance-group-map server))
      (let* ((workbox (gethash group (get-instance-group-map server)))
             (workbox-id (gethash 'id workbox))
             (message (fetch-workbox-message server workbox-id article))
             (type (alist-get 'type message))
             (metadata (parse-json-body (alist-get 'metadata message)))
             (message-body (parse-message type metadata))
             (user (alist-get 'user message))
             (from (alist-get 'name user))
             (iso-date (alist-get 'sent_at message))
             (date (iso8601-to-nov-date iso-date)))
        (with-current-buffer (or to-buffer nntp-server-buffer)
          (erase-buffer)
          (insert (format "220 %d <%s>\nFrom: %s\nSubject: %s\nDate: %s\nMessage-ID: %d\nBytes: %d\nLines: %d\n\n%s\n."
                          article
                          article
                          from
                          date
                          date
                          article
                          (length message-body)
                          (length (split-string message-body))
                          message-body)))
        (cons group article)) ;; Indicate success
      (progn
        (message "nnhelplightning-request-article: Article %s not found in group %s"
                 article group)
        nil))) ;; Indicate failure

(defun nnhelplightning-request-group (group &optional server _fast _info)
  "Request information about GROUP for the custom backend.
  Return t if the group exists, nil otherwise.

  Get data on group. This function also has the side effect of
  making group the current group.

  If fast, don’t bother to return useful data, just make group
  the current group.

  If info, it allows the backend to update the group info
  structure.

  Here’s an example of some result data and a definition of the
  same:

  `211 56 1000 1059 ifi.discussion`

  The first number is the status, which should be 211. Next is
  the total number of articles in the group, the lowest article
  number, the highest article number, and finally the group
  name. Note that the total number of articles may be less than
  one might think while just considering the highest and lowest
  article numbers, but some articles may have been canceled. Gnus
  just discards the total-number, so whether one should take the
  bother to generate it properly (if that is a problem) is left
  as an exercise to the reader. If the group contains no
  articles, the lowest article number should be reported as 1 and
  the highest as 0.
  "
  (message "nnhelplightning-request-group: Checking if group exists: %s on server: %s" group server)
  ;; if group is a key in nnhelplightning-group-map
  ;; then return true, otherwise return nil
  (if (gethash group (get-instance-group-map server))
      (progn
        (message "Group found: %s" group)
        t) ;; Return t if the group exists
    (progn
      (message "Group not found: %s" group)
      nil))) ;; Return nil if the group does not exist

(defun nnhelplightning-close-group (_group &optional _server)
  "Close group and free any resources connected to it. This will be
a no-op on most back ends.

  There should be no data returned.
  "
  (message "nnhelplightning-close-group")
  t)

(defun nnhelplightning-request-list (&optional server)
  "Populate the nntp-server-buffer with groups available on the
  nnhelplightning backend.

  Return a list of all groups available on server. And that means
  all.

  Here’s an example from a server that only carries two groups:

  ifi.test 0000002200 0000002000 y
  ifi.discussion 3324 3300 n

  On each line we have a group name, then the highest article
  number in that group, the lowest article number, and finally a
  flag. If the group contains no articles, the lowest article
  number should be reported as 1 and the highest as 0.
  "
  (message "nnhelplightning-request-list called %s" server)
  (let ((workboxes (fetch-all-workboxes server)))
    ;; store all the workboxes in a hashmap based upon a unique name
    (populate-group-map server workboxes)
    
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      ;; Write each group into the buffer in the expected format:
      ;; <GROUP-NAME> <HIGHEST-ARTICLE-NUMBER> <LOWEST-ARTICLE-NUMBER> <SUBSCRIBED-STATUS>
      ;; Iterate through workboxes and write each as a group
      (maphash (lambda (name workbox)
                 (let* ((highest (gethash 'recent-message-id workbox))
                       (lowest (- highest (gethash 'unread-messages-count workbox)))
                       (subscribed "y")) ;; always subscribed
                   (message "Adding group: %s, highest: %s, lowest: %s" name highest lowest)
                   (insert (format "%s %d %d %s\n"
                                   name
                                   (or highest 0) ;; Default to 0 if highest is nil
                                   (or lowest 1) ;; Default to 1 if lowest is nil
                                   subscribed))))
               (get-instance-group-map server)))

    t)) ;; Return t to indicate success

(defun nnhelplightning-request-post (&optional server)
  "This function should post the current buffer. It might return
  whether the posting was successful or not, but that’s not
  required. If, for instance, the posting is done asynchronously,
  it has generally not been completed by the time this function
  concludes. In that case, this function should set up some kind of
  sentinel to beep the user loud and clear if the posting could not
  be completed.

  There should be no result data from this function.
  "
  (message "nnhelplightning-request-post")
  (goto-char (point-min))
  
  ;; Ensure buffer is UTF-8 encoded
  (set-buffer-file-coding-system 'utf-8)
  
  (let ((article (buffer-substring-no-properties (point-min) (point-max))))
    (message "nnhelplightning-handle-post: Received article:\n%s" article)

    ;; Parse the article into headers and body
    (let* ((parsed-headers (mail-header-parse article))
           (from (mail-header-extract-field "from" parsed-headers))
           (subject (mail-header-extract-field "subject" parsed-headers))
           (newsgroups (mail-header-extract-field "newsgroups" parsed-headers))
           (references (mail-header-extract-field "references" parsed-headers))
           (date (mail-header-extract-field "date" parsed-headers))
           (body (mail-header-extract-body article))
           (unfilled-body (unfill-text body))
           (groups (if newsgroups
                       (split-string newsgroups ",[ \t]*" t)
                     '("default.group")))
           (first-group (car groups)))
      (message "groups %s | body %s" first-group unfilled-body)

      (if-let ((workbox (gethash first-group (get-instance-group-map server))))
          (let* ((workbox-info (fetch-workbox-info server (gethash 'id workbox)))
                 (workbox-token (alist-get 'token workbox-info))
                 (name "Marcus")
                 (date (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
                 (url (format "%s/api/v1r1/workbox/messages" (get-instance-url server)))
                 (api-key (get-instance-api-key server))
                 (json-body (json-encode `(("username" . ,name)
                                   ("type" . "Text")
                                   ("version" . 2)
                                   ("sent_at" . ,date)
                                   ("metadata" . (("message" . ,unfilled-body)))
                                   )))
                 )
            (message "json-body %s" json-body)
            (request
              url
              :type "POST"
              :headers `(("Content-Type" . "application/json")
                         ("x-helplightning-api-key" . ,api-key)
                         ("authorization" . ,workbox-token))
              :data json-body
              :parser 'json-read
              :sync t
              :success (cl-function
                        (lambda (&key data &allow-other-keys)
                          (message "Successfully posted message %s" data)
                          ))
              :error (cl-function
                      (lambda (&key error-thrown &allow-other-keys)
                        (message "Error: %s" error-thrown))))
            
            t)))))

;;
;; gnus Optional API
;;
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Optional-Back-End-Functions.html#Optional-Back-End-Functions-1
;;

(defun nnhelplightning-retrieve-groups (groups &optional server)
  "Retrieve group information for GROUPS and write to `nntp-server-buffer`.

  groups is a list of groups, and this function should request
  data on all those groups. How it does it is of no concern to
  Gnus, but it should attempt to do this in a speedy fashion.

  The return value of this function can be either active or
  group, which says what the format of the result data is. The
  former is in the same format as the data from
  nnchoke-request-list, while the latter is a buffer full of
  lines in the same format as nnchoke-request-group gives.
  "
  (message "nnhelplightning-retrieve-groups called on %s: %s" server groups)
  (let* ((deactivate-mark)
         (updated-workboxes (fetch-all-workboxes-updated-since server (get-instance-last-update server))))
    (update-group-map server updated-workboxes)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (gname groups)
        (if-let ((workbox (gethash gname (get-instance-group-map server))))
            (let* ((unread (or (gethash 'unread-messages-count workbox) 0))
                   (min-article 1) ;; Placeholder for minimum article number
                   (max-article (or (gethash 'recent-message-id workbox) unread)))
              (message "Updating group %s to %d %d %d" gname unread min-article max-article)
              (insert (format "211 %d %d %d %s\n"
                              unread
                              min-article
                              max-article
                              (string-replace " " "\\ " gname))))
          ;; Handle case where the group is not found
          (insert (format "411 no such news group: %s\n" gname)))))
    'group)) ;; Signal successful group retrieval


(defun nnhelplightning-request-update-info (group info &optional server)
  "A Gnus group info (see Group Info) is handed to the back end for
  alterations. This comes in handy if the back end really carries
  all the information (as is the case with virtual and imap
  groups). This function should destructively alter the info to
  suit its needs, and should return a non-nil value (exceptionally,
  nntp-request-update-info always returns nil not to waste the
  network resources).

  There should be no result data from this function.
  "
  (message "nnhelplightning-request-update-info")
  nil)

(defun nnhelplightning-request-type (group &optional article)
  "When the user issues commands for “sending news” (F in the
  summary buffer, for instance), Gnus has to know whether the
  article the user is following up on is news or mail. This
  function should return news if article in group is news, mail if
  it is mail and unknown if the type can’t be decided. (The article
  parameter is necessary in nnvirtual groups which might very well
  combine mail groups and news groups.) Both group and article may
  be nil.

  There should be no result data from this function.
  "
  (message "nnhelplightning-request-type")
  'news)

(defun nnhelplightning-request-set-mark (group action &optional server)
  "Set/remove/add marks on articles. Normally Gnus handles the
  article marks (such as read, ticked, expired etc.) internally,
  and store them in ~/.newsrc.eld. Some back ends (such as IMAP)
  however carry all information about the articles on the server,
  so Gnus need to propagate the mark information to the server.

  action is a list of mark setting requests, having this format:

  (RANGE ACTION MARK)

  range is a range of articles you wish to update marks
  on. action is add or del, used to add marks or remove
  marks (preserving all marks not mentioned). mark is a list of
  marks; where each mark is a symbol. Currently used marks are
  read, tick, reply, expire, killed, dormant, save, download,
  unsend, and forward, but your back end should, if possible, not
  limit itself to these.

  Given contradictory actions, the last action in the list should
  be the effective one. That is, if your action contains a
  request to add the tick mark on article 1 and, later in the
  list, a request to remove the mark on the same article, the
  mark should in fact be removed.

  An example action list:

  (((5 12 30) 'del '(tick))
   ((10 . 90) 'add '(read expire))
   ((92 94) 'del '(read)))

  The function should return a range of articles it wasn’t able
  to set the mark on (currently not used for anything).

  There should be no result data from this function.
  "
  (message "nnhelplightning-set-mark")
  nil)

(defun nnhelplightning-request-update-mark (group article mark)
  "If the user tries to set a mark that the back end doesn’t like,
  this function may change the mark. Gnus will use whatever this
  function returns as the mark for article instead of the original
  mark. If the back end doesn’t care, it must return the original
  mark, and not nil or any other type of garbage.

  The only use for this I can see is what nnvirtual does with
  it—if a component group is auto-expirable, marking an article
  as read in the virtual group should result in the article being
  marked as expirable.

  There should be no result data from this function.
  "
  (message "nnhelplightning-request-update-mark")
  mark)

(defun nnhelplightning-request-scan (&optional group server)
  "This function may be called at any time (by Gnus or anything
  else) to request that the back end check for incoming articles,
  in one way or another. A mail back end will typically read the
  spool file or query the POP server when this function is
  invoked. The group doesn’t have to be heeded—if the back end
  decides that it is too much work just scanning for a single
  group, it may do a total scan of all groups. It would be nice,
  however, to keep things local if that’s practical.
  
  There should be no result data from this function.
  "
  (message "nnhelplightning-request-scan: %s" group)
  ;; (let ((workboxes (fetch-all-workboxes)))
  ;;   ;; store all the workboxes in a hashmap based upon a unique name
  ;;   (populate-group-map workboxes)
  ;;   t))
  t)

(defun nnhelplightning-request-group-description (group &optional server)
  "The result data from this function should be a description of
  group.

  description-line = name <TAB> description eol
  name             = <string>
  description      = <text>
  "
  (message "nnhelplightning-request-group-description")
  t)

(defun nnhelplightning-request-list-newsgroups (&optional server)
  "The result data from this function should be the description of
  all groups available on the server.
  
  description-buffer = *description-line
  "
  (message "nnhelplightning-request-list-newsgroups")
  t)

(defun nnhelplightning-request-newgroups (_date &optional server)
  "The result data from this function should be all groups that were
  created after ‘date’, which is in normal human-readable date
  format (i.e., the date format used in mail and news headers, and
  returned by the function message-make-date by default). The data
  should be in the active buffer format.

  It is okay for this function to return “too many” groups; some
  back ends might find it cheaper to return the full list of
  groups, rather than just the new groups. But don’t do this for
  back ends with many groups. Normally, if the user creates the
  groups herself, there won’t be too many groups, so nnml and the
  like are probably safe. But for back ends like nntp, where the
  groups have been created by the server, it is quite likely that
  there can be many groups.
  "
  (message "nnhelplightning-request-newgroups")
  (let ((workboxes (fetch-all-workboxes server)))
    ;; store all the workboxes in a hashmap based upon a unique name
    (populate-group-map server workboxes)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert "231 New newsgroups follow\n")
      (maphash (lambda (name workbox)
                 (insert (format "%s %d %d y\n"
                                 name
                                 0 0)))
               (get-instance-group-map server)))
    t))

;; (defun nnhelplightning-request-create-group (group &optional server)
;;   "This function should create an empty group with name group.

;;   There should be no return data.
;;   "
;;   t)

(defun nnhelplightning-request-expire-articles (articles &optional group server force)
  "This function should run the expiry process on all articles in
  the articles range (which is currently a simple list of article
  numbers.) It is left up to the back end to decide how old
  articles should be before they are removed by this function. If
  force is non-nil, all articles should be deleted, no matter how
  new they are.
  
  This function should return a list of articles that it did
  not/was not able to delete.

  There should be no result data returned.
  "
  (message "nnhelplightning-request-expire-articles")
  ;; we Don't expire anything in help lightning...
  articles)

;; (defun nnhelplightning-request-move-article (article group server accept-form &optional last)
;;   "This function should move article (which is a number) from group
;;   by calling accept-form.

;;   This function should ready the article in question for moving
;;   by removing any header lines it has added to the article, and
;;   generally should “tidy up” the article. Then it should eval
;;   accept-form in the buffer where the “tidy” article is. This
;;   will do the actual copying. If this eval returns a non-nil
;;   value, the article should be removed.

;;   If last is nil, that means that there is a high likelihood that
;;   there will be more requests issued shortly, so that allows some
;;   optimizations.

;;   The function should return a cons where the car is the group
;;   name and the cdr is the article number that the article was
;;   entered as.

;;   There should be no data returned.
;;   "
;;   t)

;; (defun nnhelplightning-request-accept-article (group &optional server last)
;;   "This function takes the current buffer and inserts it into
;;   group. If last in nil, that means that there will be more calls
;;   to this function in short order.

;;   The function should return a cons where the car is the group
;;   name and the cdr is the article number that the article was
;;   entered as.

;;   The group should exist before the back end is asked to accept
;;   the article for that group.

;;   There should be no data returned.
;;   "
;;   t)

;; (defun nnhelplightning-request-replace-article (article group buffer)
;;   "This function should remove article (which is a number) from
;;   group and insert buffer there instead.

;;   There should be no data returned.
;;   "
;;   t)

;; (defun nnhelplightning-request-delete-group (group force &optional server)
;;   "This function should delete group. If force, it should really
;;   delete all the articles in the group, and then delete the group
;;   itself. (If there is such a thing as “the group itself”.)
  
;;   There should be no data returned.
;;   "
;;   t)

;; (defun nnhelplightning-request-rename-group (group new-name &optional server)
;;   "This function should rename group into new-name. All articles in
;;   group should move to new-name.
  
;;   There should be no data returned.
;;   "
;;   t)

;;
;; Internal functions
;;

(defun connect-server (server)
  (let ((url (format "%s/api/v1r1/auth/pat" (get-instance-url server)))
        (token (get-instance-pat server))
        (api-key (get-instance-api-key server))
        (response-token nil))
    (request
      url
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("x-helplightning-api-key" . ,api-key))
      :data (json-encode `(("token" . ,token)))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq response-token (alist-get 'token data))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error: %s" error-thrown))))

    ;; set the response-token
    (set-instance-token server response-token)
    (set-expiration server))
  
  t) ;; Return t to indicate success

(defun set-expiration (server)
  "Set `nnhelplightning-expiration` to the current time + 10 minutes."
    (set-instance-expiration server (time-add (current-time) (seconds-to-time (* 10 60)))))

(defun is-expired (exp)
  "Check if the current time is greater than `nnhelplightning-expiration`.
Returns t if expired, nil otherwise."
  (if exp
      (time-less-p exp (current-time))
    t)) ;; Treat as expired if expiration is nil

(defun fetch-all-workboxes (server)
  "Paginate through all the workboxes"
  (fetch-workboxes server nil nil)
  )

(defun fetch-workboxes (server after existing-workboxes)
  "Fetch workboxes recursively from the API, combining them into a single list.
AFTER is the pagination cursor (or nil to start).
EXISTING-WORKBOXES is the accumulated list of workboxes."
  (let ((url (format "%s/api/v1r1/workboxes" (get-instance-url server)))
        (api-key (get-instance-api-key server))
        (token (get-instance-token server))
        (new-workboxes nil)
        (next-after nil))
    ;; Make the GET request
    (request
      url
      :type "GET"
      :headers `(("Content-Type" . "application/json")
                 ("x-helplightning-api-key" . ,api-key)
                 ("authorization" . ,token))
      :params (let ((params '(("status" . "OPEN"))))
                (if after
                    (cons `("after" . ,after) params)
                  params))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq new-workboxes (seq-into (alist-get 'entries data) 'list)) ;; convert from a vector to a list
                  (setq next-after (alist-get 'after data))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Failed to fetch workboxes: %s" error-thrown))))
    ;; Combine the workboxes
    (let ((combined-workboxes (append existing-workboxes new-workboxes)))
      ;; Recurse if there's a next page; otherwise return the combined list
      (if next-after
          (fetch-workboxes server next-after combined-workboxes)
                combined-workboxes))))

(defun fetch-all-workboxes-updated-since (server timestamp)
  "Paginate through all workboxes that have been updated
after timestampe"
  (set-instance-last-update server (current-time))
  (fetch-workboxes-updated-since server timestamp nil nil)
  )

(defun fetch-workboxes-updated-since (server timestamp after existing-workboxes)
  "Fetch workboxes recursively from the API, combining them into a single list.
TIMESTAMP only fetch workboxes newer than timestamp
AFTER is the pagination cursor (or nil to start).
EXISTING-WORKBOXES is the accumulated list of workboxes."
  (let ((url (format "%s/api/v1r1/workboxes" (get-instance-url server)))
        (api-key (get-instance-api-key server))
        (token (get-instance-token server))
        (new-workboxes nil)
        (next-after nil))
    ;; Make the GET request
    (request
      url
      :type "GET"
      :headers `(("Content-Type" . "application/json")
                 ("x-helplightning-api-key" . ,api-key)
                 ("authorization" . ,token))
      :params (let ((params '(("status" . "OPEN"))))
                (if after
                    (cons `("after" . ,after) params)
                  params))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq new-workboxes
                        (seq-filter (lambda (x)
                                      (is-date-after
                                       (parse-iso8601-time-string
                                        (alist-get 'last_active_at x))
                                       timestamp))
                                    (seq-into (alist-get 'entries data) 'list))) ;; convert from a vector to a list
                  
                  ;; if length of new-workboxes != length of data['entries]
                  ;; then stop, otherwise continue
                  (if (not (= (length new-workboxes) (length (alist-get 'entries data))))
                      (setq next-after (alist-get 'after data)))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Failed to fetch workboxes: %s" error-thrown))))
    ;; Combine the workboxes
    (let ((combined-workboxes (append existing-workboxes new-workboxes)))
      ;; Recurse if there's a next page; otherwise return the combined list
      (if next-after
          (fetch-workboxes-updated-since server timestamp next-after combined-workboxes)
                combined-workboxes))))


(defun fetch-workbox-recent-message-id (server workbox-id)
  "Fetch the recent messages for a workbox.
WORKBOX-ID is the id for the workbox."
  (let* ((workbox-info (fetch-workbox-info server workbox-id))
         (workbox-token (alist-get 'token workbox-info))
         (url (format "%s/api/v1r1/workbox/messages/recent_history" (get-instance-url server)))
         (api-key (get-instance-api-key server))
         (latest-message-id nil))
    (request
      url
      :type "GET"
      :headers `(("Content-Type" . "application/json")
                 ("x-helplightning-api-key" . ,api-key)
                 ("authorization" . ,workbox-token))
      :params `(("limit" . "1"))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((first-entry (aref data 0)))  ;; Access the first entry
                    (setq last-message-id (alist-get 'id first-entry)))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Failed to fetch workbox messages: %s" error-thrown))))
    
    last-message-id))

(defun fetch-workbox-unread-count (server workbox-id)
  "Fetch the recent messages for a workbox.
WORKBOX-ID is the id for the workbox."
  (let ((workbox-info (fetch-workbox-info server workbox-id)))
    (alist-get 'unread_messages_count workbox-info)))

(defun fetch-workbox-message (server workbox-id message-id)
  "Fetch a specific message
WORKBOX-ID is the id for the workbox
MESSAGE-ID is the id of the message."
  (let* ((workbox-info (fetch-workbox-info server workbox-id))
         (workbox-token (alist-get 'token workbox-info))
         (url (format "%s/api/v1r1/workbox/messages/%s" (get-instance-url server) message-id))
         (api-key (get-instance-api-key server))
         (message nil))
    (request
      url
      :type "GET"
      :headers `(("Content-Type" . "application/json")
                 ("x-helplightning-api-key" . ,api-key)
                 ("authorization" . ,workbox-token))
      :params `(("limit" . "1"))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((type (alist-get 'type data))
                         (metadata (parse-json-body (alist-get 'metadata data)))
                         (body (parse-message type metadata)))
                    (message "Message Body: %s: %s" body metadata)
                    (setq message data))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Failed to fetch workbox messages: %s" error-thrown))))
    
    message))

(defun fetch-all-workbox-articles (server workbox-id)
  (let* ((workbox-info (fetch-workbox-info server workbox-id))
         (workbox-token (alist-get 'token workbox-info)))
    (fetch-workbox-articles server workbox-token nil nil)))

(defun fetch-workbox-articles (server workbox-token after existing-messages)
  "Fetch workbox messages recursively from the API, combining them into a
single list.
WORKBOX-TOKEN is the token of the workbox.
AFTER is the pagination cursor (nil to start).
EXISTING-MESSAGES is the accumulated list of messages."
  (let ((url (format "%s/api/v1r1/workbox/messages" (get-instance-url server)))
        (api-key (get-instance-api-key server))
        (new-messages nil)
        (next-after nil))
    ;; Make the GET request
    (request
      url
      :type "GET"
      :headers `(("Content-Type" . "application/json")
                 ("x-helplightning-api-key" . ,api-key)
                 ("authorization" . ,workbox-token))
      :params (let ((params '(())))
                (if after
                    (cons `("after" . ,after) params)
                  params))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq new-messages (seq-into (alist-get 'entries data) 'list)) ;; convert from a vector to a list
                  (setq next-after (alist-get 'after data))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Failed to fetch workbox messages: %s" error-thrown))))
    ;; Combine the workboxes
    (let ((combined-messages (append existing-messages new-messages)))
      ;; Recurse if there's a next page; otherwise return the combined list
      (if next-after
          (fetch-workbox-articles server workbox-token next-after combined-messages)
                combined-messages))))

(defun fetch-workbox-info (server workbox-id)
  "Fetch a workbox info based upon its id
WORKBOX-ID is the id of the workbox to fetch."
  (let ((url (format "%s/api/v1r1/workboxes/%d" (get-instance-url server) workbox-id))
        (api-key (get-instance-api-key server))
        (token (get-instance-token server))
        (workbox nil))
    ;; Make the GET request
    (request
      url
      :type "GET"
      :headers `(("Content-Type" . "application/json")
                 ("x-helplightning-api-key" . ,api-key)
                 ("authorization" . ,token))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq workbox data)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "Failed to fetch workbox info: %s" error-thrown))))
    workbox))


(defun populate-group-map (server groups)
  "Populate `nnhelplightning-group-map` with metadata from GROUPS.
Each entry in GROUPS should be the response from fetch-all-workboxes"
  (let ((group-map (make-hash-table :test 'equal)))
    (dolist (group groups)
      (let* ((id (alist-get 'id group))
             (title (alist-get 'title group))
             (unique-name (sanitize-group-name (format "%s-%s" id title)))
             (unread-messages-count (alist-get 'unread_messages_count group))
             (recent-message-id (fetch-workbox-recent-message-id server id))
             (metadata (make-hash-table :test 'equal))) ;; Metadata map
        ;; Populate the metadata hash map
        (puthash 'id id metadata)
        (puthash 'title title metadata)
        (puthash 'unread-messages-count unread-messages-count metadata)
        (puthash 'recent-message-id recent-message-id metadata)
        
        ;; Add to group map
        (puthash unique-name metadata group-map)))

    (set-instance-group-map server group-map)))

(defun update-group-map (server groups)
  "Update `nnhelplightning-group-map`"
  (let ((group-map (get-instance-group-map server)))
    (dolist (group groups)
      (let* ((id (alist-get 'id group))
             (title (alist-get 'title group))
             (unique-name (sanitize-group-name (format "%s-%s" id title)))
             (unread-messages-count (alist-get 'unread_messages_count group))
             (recent-message-id (fetch-workbox-recent-message-id server id))
             (metadata (or (gethash unique-name group-map) (make-hash-table :test 'equal))))
        ;; Update the metadata hash map
        (puthash 'id id metadata)
        (puthash 'title title metadata)
        (puthash 'unread-message-count unread-messages-count metadata)
        (puthash 'recent-message-id recent-message-id metadata)

        (puthash unique-name metadata group-map)))
    (set-instance-group-map server group-map)))


(defun sanitize-group-name (name)
  "Sanitize group NAME to ensure GNUS compatibility.
Replaces spaces with underscores and removes special characters like #, :, ), (, and /."
  (let* ((sanitized-name0 (replace-regexp-in-string "[[:space:]]+" "-" name))
         (sanitized-name1 (replace-regexp-in-string "[#/:()]" "-" sanitized-name0))
         (sanitized-name (replace-regexp-in-string "[^\x00-\x7F]" "" sanitized-name1)))
    sanitized-name))

(defun is-date-after (first second)
  (time-less-p second first))

(defun iso8601-to-nov-date (iso-date)
  "Convert ISO 8601 date string ISO-DATE to NOV-compatible date format."
  (let ((parsed-time (parse-iso8601-time-string iso-date)))
        (format-time-string "%a, %d %b %Y %H:%M:%S %z" parsed-time)))

(defun parse-json-body (json-body)
  "Parse the JSON string JSON-BODY into a Lisp data structure."
  (json-parse-string json-body
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

;; Function to parse email headers
(defun mail-header-parse (article)
  "Parse ARTICLE into a list of header fields.
Returns an alist where each element is (FIELD . VALUE)."
  (let ((headers '())
        (in-headers t)
        (current-field nil)
        (current-value ""))
    (dolist (line (split-string article "\n"))
      (if in-headers
          (if (string-match "^\\([^:]+\\):[ \t]*\\(.*\\)$" line)
              (progn
                ;; Save the previous header
                (when current-field
                  (push (cons (downcase current-field) (string-trim current-value)) headers))
                ;; Start a new header
                (setq current-field (match-string 1 line))
                (setq current-value (match-string 2 line)))
            (if (and current-field (string-match "^[ \t]+" line))
                ;; Continuation of the previous header
                (setq current-value (concat current-value " " (string-trim line)))
              ;; End of headers
              (setq in-headers nil)))
        ;; In body; do nothing
        ))
    ;; Add the last header
    (when current-field
      (push (cons (downcase current-field) (string-trim current-value)) headers))
    (reverse headers)))

(defun mail-header-extract-field (field headers)
  "Extract FIELD value from parsed HEADERS.
HEADERS is assumed to be an alist of header fields."
  (cdr (assoc (downcase field) headers)))

(defun mail-header-extract-body (article)
  "Extract the body from ARTICLE string.
Assumes headers and body are separated by a blank line."
  (let ((parts (split-string article "\n\n" t)))
    (if (> (length parts) 1)
        (mapconcat 'identity (cdr parts) "\n\n") ; Rejoin in case of multiple parts
      "")))

(defun create-instance (server)
  (message "create-instance %s" server)
  (let ((instance (make-hash-table :test 'equal)))
    (puthash 'name server instance)
    (puthash 'url nil instance)
    (puthash 'api-key nil instance)
    (puthash 'pat nil instance)
    (puthash 'token nil instance)
    (puthash 'expiration nil instance)
    (puthash 'group-map (make-hash-table :test 'equal) instance)
    (puthash 'last-update nil instance)
    (puthash 'status-string "" instance)
    instance))

(defun get-instance (server)
  (gethash server nnhelplightning-instances))

(defun set-instance-url (server url)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'url url instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-url (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
    (gethash 'url instance)))

(defun set-instance-api-key (server api-key)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'api-key api-key instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-api-key (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
    (gethash 'api-key instance)))

(defun set-instance-pat (server pat)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'pat pat instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-pat (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
    (gethash 'pat instance)))

(defun set-instance-token (server token)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'token token instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-token (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
    (gethash 'token instance)))

(defun set-instance-expiration (server expiration)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'expiration expiration instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-expiration (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
    (gethash 'expiration instance)))

(defun set-instance-group-map (server group-map)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'group-map group-map instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-group-map (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
    (gethash 'group-map instance)))

(defun set-instance-last-update (server last-update)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'last-update last-update instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-last-update (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
    (gethash 'last-update instance)))

(defun set-instance-status-string (server status-string)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (progn
        (puthash 'status-string status-string instance)
        (puthash server instance nnhelplightning-instances))
    nil))

(defun get-instance-status-string (server)
  (if-let ((instance (gethash server nnhelplightning-instances)))
      (gethash 'status-string instance)
    ""))

(defun is-supported-type (type)
  (member type '("Audio" "Document" "Image" "QRMessage" "Text" "Video")))

(defun parse-message (type metadata)
  (pcase type
    ("Audio" (parse-audio-message metadata))
    ("Document" (parse-document-message metadata))
    ("Image" (parse-image-message metadata))
    ("QRMessage" (parse-qrmessage-message metadata))
    ("Text" (parse-text-message metadata))
    ("Video" (parse-video-message metadata))
    (_ "")))

(defun parse-audio-message (metadata)
  (let ((url (alist-get 'url metadata))
        (filename (alist-get 'filename metadata)))
    (format "%s: %s" filename url)))

(defun parse-document-message (metadata)
  (let ((url (alist-get 'url metadata))
        (filename (alist-get 'filename metadata)))
    (format "%s: %s" filename url)))

(defun parse-image-message (metadata)
  (let ((url (alist-get 'url metadata))
        (filename (alist-get 'filename metadata)))
    (format "%s: %s" filename url)))

(defun parse-qrmessage-message (metadata)
  (let ((message (alist-get 'message metadata))
        (title (alist-get 'title metadata)))
    (format "%s: %s" title message)))

(defun parse-text-message (metadata)
  (let ((message (alist-get 'message metadata)))
    message))

(defun parse-video-message (metadata)
  (let ((url (alist-get 'url metadata))
        (filename (alist-get 'filename metadata)))
    (format "%s: %s" filename url)))

(defun unfill-text (text &optional fill-column-v)
  "Remove soft newlines (inserting spaces) in the given TEXT while preserving hard newlines.

  This function processes the text paragraph by paragraph, working in reverse order,
  and unwrapping lines that have been broken by auto-fill-mode.
  
  STRATEGY:
  1. The input TEXT is split into paragraphs using double newlines (`\n\n`) as delimiters.
  2. For each paragraph:
     - The paragraph is split into lines using single newlines (`\n`).
     - The lines are processed in reverse order.
     - For each newline:
       - The function calculates the combined length of the current line and the next word.
       - If this length exceeds the `fill-column`, the newline is likely a soft newline inserted by `auto-fill-mode`, and it is replaced with a space.
       - Otherwise, the newline is preserved as a hard newline (e.g., indicating the end of a list item or a user-entered line break).
  3. After processing, the lines are rejoined with appropriate newlines to preserve paragraph boundaries.

  Returns the processed TEXT with soft newlines removed and hard newlines preserved.

  If FILL-COLUMN-V is not provided, it uses the current `fill-column` value."
  (let* ((fill-column (or fill-column-v fill-column))  ;; Use provided fill-column or the global one
         (paragraphs (split-string text "\n\n" t)))  ;; Split text into paragraphs by double newlines
    (mapconcat
     (lambda (paragraph)
       (let* ((lines (split-string paragraph "\n" t))  ;; Split the paragraph into lines
              (rev-lines (reverse lines))
              result)
         ;; Use reduce to process lines in reverse order and accumulate the result
         (setq result (seq-reduce
          (lambda (acc line)
            (let* ((line-length (length line))
                   (next-word-length (if (alist-get 'prev acc)
                                         (length (car (split-string (alist-get 'prev acc))))
                                       0))
                   (merged-line (if (> (+ line-length next-word-length 1) fill-column)
                                    (concat line " " (alist-get 'acc acc))  ;; Merge the lines
                                  (if (= next-word-length 0)
                                      (progn
                                        line)
                                    (progn
                                      (concat line "\n" (alist-get 'acc acc)))))))  ;; Preserve newline

              (list (cons 'prev line)  ;; Set 'prev' to the current line
                    (cons 'acc merged-line))))  ;; Set 'acc' to the merged line
          rev-lines  ;; Reverse the lines to process in reverse order
          '((acc . "") (prev . nil))))  ;; Initial accumulator

         (alist-get 'acc result)
        ))
     paragraphs "\n\n")))  ;; Preserve hard newlines between paragraphs

(provide 'nnhelplightning)



;;; nnhelplightning.el ends here
