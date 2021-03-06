(ns uxbox.db
  (:require [reagent.core :as reagent :refer [atom]]))

(def initial-state {;; Location
                    :location [:login]

                    ;; User
                    :user {:fullname "Michael Buchannon"
                           :avatar "/images/avatar.jpg"}

                    ;; Projects
                    :project nil
                    :project-sort-order :name
                    :project-orderings {
                      :name "name"
                      :last-update "date updated"
                      :created "date created"
                    }
                    :project-layouts {
                      :mobile {:name "Mobile" :width 320 :height 480}
                      :tablet {:name "Tablet" :width 1024 :height 768}
                      :notebook {:name "Notebook" :width 1366 :height 768}
                      :desktop {:name "Desktop" :width 1920 :height 1080}
                    }
                    :projects-list {}
                    :new-project-defaults {
                      :name ""
                      :width 1920
                      :height 1080
                      :layout :desktop
                    }
                    :new-project-name ""
                    :project-bar-visible? false

                    ;; Pages
                    :page nil
                    :new-page-name ""
                    :editing-pages {}
                    :adding-new-page? false

                    ;; Activity
                    :activity []

                    ;; Header
                    :user-menu-open? false

                    ;; Forms
                    :login-form :login

                    ;; Lightbox
                    :lightbox nil

                    ;; Workspace
                    :workspace-defaults {:selected-tool nil
                                         :selected-element 0
                                         :grid? false
                                         :zoom 1}
                    :workspace {:selected-tool nil
                                :selected-groups #{}
                                :selected-element 0
                                :grid? false
                                :zoom 1}

                    :current-icons-set :material-design-actions
                    :default-open-setting-boxes #{:tools :layers}
                    :open-setting-boxes #{:tools :layers}

                    ;; Browser
                    :scroll {:top 0 :left 0}})

(defonce app-state (atom initial-state))
