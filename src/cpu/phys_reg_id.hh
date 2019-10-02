//=============================================================================
// phys_reg_id.hh
//=============================================================================
//
// Author: Tuan Ta
// Date:   19/08/29

#ifndef __CPU_PHYS_REG_ID_HH__
#define __CPU_PHYS_REG_ID_HH__

#include "arch/types.hh"
#include "base/types.hh"
#include "cpu/reg_class.hh"

/** Physical register index type.
 * Although the Impl might be a better for this, but there are a few classes
 * that need this typedef yet are not templated on the Impl.
 */
using PhysRegIndex = short int;

/** Physical register ID.
 * Like a register ID but physical. The inheritance is private because the
 * only relationship between this types is functional, and it is done to
 * prevent code replication. */
class PhysRegId : private RegId {
  private:
    PhysRegIndex flatIdx;

  public:
    explicit PhysRegId() : RegId(IntRegClass, -1), flatIdx(-1) {}

    /** Scalar PhysRegId constructor. */
    explicit PhysRegId(RegClass _regClass, PhysRegIndex _regIdx,
              PhysRegIndex _flatIdx)
        : RegId(_regClass, _regIdx), flatIdx(_flatIdx)
    {}

    /** Vector PhysRegId constructor (w/ elemIndex). */
    explicit PhysRegId(RegClass _regClass, PhysRegIndex _regIdx,
              ElemIndex elem_idx, PhysRegIndex flat_idx)
        : RegId(_regClass, _regIdx, elem_idx), flatIdx(flat_idx) { }

    /** Visible RegId methods */
    /** @{ */
    using RegId::index;
    using RegId::classValue;
    using RegId::isZeroReg;
    using RegId::className;
    using RegId::elemIndex;
     /** @} */
    /**
     * Explicit forward methods, to prevent comparisons of PhysRegId with
     * RegIds.
     */
    /** @{ */
    bool operator<(const PhysRegId& that) const {
        return RegId::operator<(that);
    }

    bool operator==(const PhysRegId& that) const {
        return RegId::operator==(that);
    }

    bool operator!=(const PhysRegId& that) const {
        return RegId::operator!=(that);
    }
    /** @} */

    /** @return true if it is an integer physical register. */
    bool isIntPhysReg() const { return isIntReg(); }

    /** @return true if it is a floating-point physical register. */
    bool isFloatPhysReg() const { return isFloatReg(); }

    /** @Return true if it is a  condition-code physical register. */
    bool isCCPhysReg() const { return isCCReg(); }

    /** @Return true if it is a vector physical register. */
    bool isVectorPhysReg() const { return isVecReg(); }

    /** @Return true if it is a vector element physical register. */
    bool isVectorPhysElem() const { return isVecElem(); }

    /** @return true if it is a vector predicate physical register. */
    bool isVecPredPhysReg() const { return isVecPredReg(); }

    /** @Return true if it is a  condition-code physical register. */
    bool isMiscPhysReg() const { return isMiscReg(); }

    /**
     * Returns true if this register is always associated to the same
     * architectural register.
     */
    bool isFixedMapping() const
    {
        return !isRenameable();
    }

    /** Flat index accessor */
    const PhysRegIndex& flatIndex() const { return flatIdx; }

    static PhysRegId elemId(const PhysRegId* vid, ElemIndex elem)
    {
        assert(vid->isVectorPhysReg());
        return PhysRegId(VecElemClass, vid->index(), elem);
    }
};

/** Constant pointer definition.
 * PhysRegIds only need to be created once and then we can just share
 * pointers */
using PhysRegIdPtr = const PhysRegId*;

#endif // __CPU_PHYS_REG_ID_HH__
