(ns ^:no-doc hooks.farolero
  (:require
   [clj-kondo.hooks-api :as api]))

(defn restart-and-handler-case
  [{:keys [node]}]
  (let [[expr & bindings] (rest (:children node))
        bindings (mapcat (fn [{[name bindings & body] :children}]
                           [name
                            (api/list-node
                             (list* (api/token-node `let)
                                    (api/vector-node
                                     (vec (interleave (:children bindings)
                                                      (repeat (api/list-node
                                                               (list
                                                                (api/token-node `first)
                                                                (api/token-node nil)))))))
                                    body))])
                         bindings)
        new-node (api/list-node
                  (list*
                   (api/token-node `case)
                   expr
                   bindings))]
    {:node new-node}))

(defn block
  [{:keys [node]}]
  (let [[name & body] (rest (:children node))
        new-node (api/list-node
                  (list
                   (api/token-node `let)
                   (api/vector-node [name (api/list-node (list (api/token-node `quote)
                                                               (api/token-node `val)))])
                   (api/list-node
                    (list
                     (api/token-node 'try)
                     (api/list-node
                      (list*
                       (api/token-node 'do)
                       body))
                     (api/list-node (list (api/token-node 'finally)))))))]
    {:node new-node}))

(defn tagbody
  [{:keys [node]}]
  (let [body-elts (rest (:children node))
        label-nodes (->> body-elts
                         (filter api/token-node?)
                         (filter (comp symbol? api/sexpr)))
        labels (mapcat #(list % (api/list-node (list (api/token-node `quote) %))) label-nodes)
        body (filter (complement (set label-nodes)) body-elts)
        new-node (api/list-node
                  (list*
                   (api/token-node `let)
                   (api/vector-node (vec labels))
                   body))]
    {:node new-node}))
