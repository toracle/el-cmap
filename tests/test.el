(require 'ert)

(require 'el-cmap)

(require 'test-el-cmap-repr)

(setq fixture-graph
      '(:config (:graph nil
                        :node (:shape "record"
                                      :fillcolor "#eeeeee")
                        :edge nil)
                :digraph (:nodes (("Gerald Weinberg" "Gerald Weinberg")
                                  ("leadership" "leadership")
                                  ("process" "process")
                                  ("environment" "environment")
                                  ("people" "people")
                                  ("strength" "strength")
                                  ("motivator" "motivator")
                                  ("organizer" "organizer")
                                  ("problem-solver" "problem-solver"))
                                 :edges (("Gerald Weinberg" "leadership" "defines")
                                         ("leadership" "process" "is a")
                                         ("process" "environment" "creating an")
                                         ("environment" "people" "in which")
                                         ("people" "empowered" "become")
                                         ("people" "strength" "who has")
                                         ("strength" "motivator" "as a")
                                         ("strength" "organizer" "as a")
                                         ("strength" "problem-solver" "as a")
                                         )))
      )
