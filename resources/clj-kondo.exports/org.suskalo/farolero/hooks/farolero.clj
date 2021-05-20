(ns hooks.farolero
  (:require
   [clj-kondo.hooks-api :as api]))

(defn restart-and-handler-case
  [{:keys [node]}]
  (let [[expr & bindings] (rest (:children node))
        bindings (mapcat (fn [{[name args & body] :children}]
                           [name
                            (api/list-node
                             (list* (api/token-node 'fn)
                                    args
                                    body))])
                         bindings)
        new-node (api/list-node
                  (list
                   (api/token-node 'binding)
                   (api/vector-node (vec bindings))
                   expr))]
    {:node new-node}))

(defn block
  [{:keys [node]}]
  (let [[name & body] (rest (:children node))
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   (api/vector-node [name (api/list-node (list (api/token-node 'quote)
                                                               (api/token-node 'val)))])
                   body))]
    {:node new-node}))

(defn tagbody
  [{:keys [node]}]
  (let [body-elts (rest (:children node))
        label-nodes (->> body-elts
                         (filter api/token-node?)
                         (filter (comp symbol? api/sexpr)))
        labels (mapcat #(list % (api/list-node (list (api/token-node 'quote) %))) label-nodes)
        body (filter (complement (set label-nodes)) body-elts)
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   (api/vector-node (vec labels))
                   body))]
    {:node new-node}))
