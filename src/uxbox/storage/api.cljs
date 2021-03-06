(ns uxbox.storage.api
  (:require [uxbox.storage.core :refer [insert-event]]
            [uxbox.storage.atoms :refer [ projects-view pages-view activity-view groups-view shapes-view]]
            [uxbox.shapes.core :refer [move-delta]]))

(defn get-activity
      [username]
      (reverse (get @activity-view username)))

(defn get-projects
    [username]
    @projects-view)

(defn get-project [uuid]
    (get @projects-view uuid))

(defn get-pages
    [project-uuid]
    (into {} (filter #(= (:project-uuid (second %))) @pages-view)))

(defn get-first-page [uuid]
    (let [pages (get-pages uuid)]
      ;; TODO get the first by a valid order
      (first (vals pages))))

(defn get-page
    [page-uuid]
    (get @pages-view page-uuid))

(defn get-groups
    [project-uuid page-uuid]
    (into {} (filter #(and (= (:project-uuid (second %)) project-uuid) (= (:page-uuid (second %)) page-uuid)) @groups-view)))

(defn get-shapes
    [project-uuid page-uuid]
    (into {} (filter #(and (= (:project-uuid (second %)) project-uuid) (= (:page-uuid (second %)) page-uuid)) @shapes-view)))

(defn get-shape
    [shape-uuid]
    (get @shapes-view shape-uuid))

(defn create-project [project]
    (insert-event {:type :create-project :data project}))

(defn create-page
    [page]
    (insert-event {:type :create-page :data page}))

(defn change-page-title
    [project-uuid page title]
    (insert-event {:type :change-page-title :data {:project-uuid project-uuid :page-uuid (:uuid page) :old-title (:title page) :new-title title}}))

(defn delete-page
    [project-uuid page]
    (insert-event {:type :delete-page :data {:project-uuid project-uuid :page-uuid (:uuid page)}}))

(defn delete-project
    [uuid]
    (insert-event {:type :delete-project :data {:project-uuid uuid}}))

(defn create-shape
    [project-uuid page-uuid shape-uuid shape]
    (let [shape-data (assoc shape :project-uuid project-uuid :page-uuid page-uuid :uuid shape-uuid)]
      (insert-event {:type :create-shape :data shape-data})))

(defn remove-shape
    [project-uuid page-uuid shape-uuid]
    (insert-event {:type :delete-shape :data {:project-uuid project-uuid :page-uuid page-uuid :shape-uuid shape-uuid}}))

(defn move-shape
    [project-uuid page-uuid shape-uuid deltax deltay]
    (insert-event {:type :move-shape :data {:project-uuid project-uuid :page-uuid page-uuid :shape-uuid shape-uuid :delta-x deltax :delta-y deltay}}))

(defn change-shape-attr
    [project-uuid page-uuid shape-uuid attr value]
    (insert-event {:type :change-shape-attr :data {:project-uuid project-uuid :page-uuid page-uuid :shape-uuid shape-uuid :attr attr :value value}}))

(defn create-group
    [project-uuid page-uuid group-uuid group]
    (let [group-data (assoc group :project-uuid project-uuid :page-uuid page-uuid :uuid group-uuid)]
      (insert-event {:type :create-group :data group-data})))

(defn remove-group
    [project-uuid page-uuid group-uuid]
    (insert-event {:type :delete-group :data {:project-uuid project-uuid :page-uuid page-uuid :group-uuid group-uuid}}))

(defn toggle-group-visibility
    [group-uuid]
    (insert-event {:type :toggle-group-visibility :data {:group-uuid group-uuid}}))

(defn toggle-group-lock
    [group-uuid]
    (insert-event {:type :toggle-group-lock :data {:group-uuid group-uuid}}))

(defn move-group-up
    [group-uuid]
    (insert-event {:type :move-group-up :data {:group-uuid group-uuid}}))

(defn move-group-down
    [group-uuid]
    (insert-event {:type :move-group-down :data {:group-uuid group-uuid}}))
