package boomerang.results;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import boomerang.jimple.Field;
import boomerang.jimple.Statement;
import boomerang.jimple.Val;
import boomerang.solver.AbstractBoomerangSolver;
import boomerang.util.AccessPath;
import sync.pds.solver.SyncPDSUpdateListener;
import sync.pds.solver.nodes.GeneratedState;
import sync.pds.solver.nodes.INode;
import sync.pds.solver.nodes.Node;
import wpds.impl.Transition;
import wpds.impl.Weight;
import wpds.impl.WeightedPAutomaton;
import wpds.interfaces.Empty;
import wpds.interfaces.WPAStateListener;
import wpds.interfaces.WPAUpdateListener;

public class ExtractAllAliasListener<W extends Weight> implements SyncPDSUpdateListener<Statement, Val> {
    private final Set<AccessPath> results;
    private final Statement stmt;
    private AbstractBoomerangSolver<W> fwSolver;

    public ExtractAllAliasListener(AbstractBoomerangSolver<W> fwSolver, Set<AccessPath> results, Statement stmt) {
        this.fwSolver = fwSolver;
        this.results = results;
        this.stmt = stmt;
    }

    @Override
    public void onReachableNodeAdded(Node<Statement, Val> reachableNode) {
        if (reachableNode.stmt().equals(stmt)) {
            Val base = reachableNode.fact();
            final INode<Node<Statement, Val>> allocNode = fwSolver.getFieldAutomaton().getInitialState();
            fwSolver.getFieldAutomaton()
                    .registerListener(new WPAUpdateListener<Field, INode<Node<Statement, Val>>, W>() {
                        @Override
                        public void onWeightAdded(Transition<Field, INode<Node<Statement, Val>>> t, W w,
                                WeightedPAutomaton<Field, INode<Node<Statement, Val>>, W> aut) {
                            if (t.getStart().fact().stmt().equals(stmt) && !(t.getStart() instanceof GeneratedState)
                                    && t.getStart().fact().fact().equals(base)) {
                                if (t.getLabel().equals(Field.empty())) {
                                    if (t.getTarget().equals(allocNode)) {
                                        results.add(new AccessPath(base));
                                    }
                                }
                                List<Transition<Field, INode<Node<Statement, Val>>>> fields = Lists.newArrayList();
                                if (!(t.getLabel() instanceof Empty)) {
                                    fields.add(t);
                                }
                                fwSolver.getFieldAutomaton().registerListener(new ExtractAccessPathStateListener(
                                        t.getTarget(), allocNode, base, fields, results));
                            }
                        }
                    });
        }
    }

    class ExtractAccessPathStateListener extends WPAStateListener<Field, INode<Node<Statement, Val>>, W> {

        private INode<Node<Statement, Val>> allocNode;
        private Collection<Transition<Field, INode<Node<Statement, Val>>>> fields;
        private Set<AccessPath> results;
        private Val base;

        public ExtractAccessPathStateListener(INode<Node<Statement, Val>> state, INode<Node<Statement, Val>> allocNode,
                Val base, Collection<Transition<Field, INode<Node<Statement, Val>>>> fields, Set<AccessPath> results) {
            super(state);
            this.allocNode = allocNode;
            this.base = base;
            this.fields = fields;
            this.results = results;
        }

        @Override
        public void onOutTransitionAdded(Transition<Field, INode<Node<Statement, Val>>> t, W w,
                WeightedPAutomaton<Field, INode<Node<Statement, Val>>, W> weightedPAutomaton) {
            if (t.getLabel().equals(Field.epsilon()))
                return;
            Collection<Transition<Field, INode<Node<Statement, Val>>>> copiedFields = (fields instanceof Set
                    ? Sets.newHashSet(fields) : Lists.newArrayList(fields));
            if (!t.getLabel().equals(Field.empty())) {
                if (copiedFields.contains(t)) {
                    copiedFields = Sets.newHashSet(fields);
                }
                if (!(t.getLabel() instanceof Empty))
                    copiedFields.add(t);
            }
            if (t.getTarget().equals(allocNode)) {

                results.add(new AccessPath(base, convert(copiedFields)));
            }
            weightedPAutomaton.registerListener(
                    new ExtractAccessPathStateListener(t.getTarget(), allocNode, base, copiedFields, results));
        }

        private Collection<Field> convert(Collection<Transition<Field, INode<Node<Statement, Val>>>> fields) {
            Collection<Field> res;
            if (fields instanceof List) {
                res = Lists.newArrayList();
            } else {
                res = Sets.newHashSet();
            }
            for (Transition<Field, INode<Node<Statement, Val>>> f : fields) {
                res.add(f.getLabel());
            }
            return res;
        }

        @Override
        public void onInTransitionAdded(Transition<Field, INode<Node<Statement, Val>>> t, W w,
                WeightedPAutomaton<Field, INode<Node<Statement, Val>>, W> weightedPAutomaton) {
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = super.hashCode();
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((allocNode == null) ? 0 : allocNode.hashCode());
            result = prime * result + ((base == null) ? 0 : base.hashCode());
            // result = prime * result + ((fields == null) ? 0 : fields.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!super.equals(obj))
                return false;
            if (getClass() != obj.getClass())
                return false;
            ExtractAccessPathStateListener other = (ExtractAccessPathStateListener) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (allocNode == null) {
                if (other.allocNode != null)
                    return false;
            } else if (!allocNode.equals(other.allocNode))
                return false;
            if (base == null) {
                if (other.base != null)
                    return false;
            } else if (!base.equals(other.base))
                return false;
            // if (fields == null) {
            // if (other.fields != null)
            // return false;
            // } else if (!fields.equals(other.fields))
            // return false;
            return true;
        }

        private ExtractAllAliasListener getOuterType() {
            return ExtractAllAliasListener.this;
        }

    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((fwSolver == null) ? 0 : fwSolver.hashCode());
        result = prime * result + ((stmt == null) ? 0 : stmt.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ExtractAllAliasListener other = (ExtractAllAliasListener) obj;
        if (fwSolver == null) {
            if (other.fwSolver != null)
                return false;
        } else if (!fwSolver.equals(other.fwSolver))
            return false;
        if (stmt == null) {
            if (other.stmt != null)
                return false;
        } else if (!stmt.equals(other.stmt))
            return false;
        return true;
    }
}