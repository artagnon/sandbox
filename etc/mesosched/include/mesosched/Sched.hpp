#pragma once

#include "mesosched/TupleParser.hpp"
#include <map>
#include <set>
#include <sstream>
#include <vector>
#include <string>

namespace mesosched {
    class NoJobs : public std::runtime_error {
    public:
        NoJobs() : std::runtime_error("No jobs left") {};
    };

    class NodesTooSmall : public std::runtime_error {
    public:
        NodesTooSmall() : std::runtime_error("No single node has enough resources for job") {};
    };

    struct Resource {
        size_t fId;
        size_t fAvailable;

        Resource(size_t aId, size_t aAvailable) : fId(aId), fAvailable(aAvailable) {}

        /// Required for some std::map things.
        bool operator<(const Resource& aRhs) const {
            return fId < aRhs.fId;
        }

        friend std::ostream& operator<<(std::ostream& os, const Resource& aResource) {
            os << std::to_string(aResource.fId) << ":" << std::to_string(aResource.fAvailable);
            return os;
        }
    };

    struct Job {
        size_t fId;
        size_t fResource;
        size_t fTimestep;

        Job(size_t aId, size_t aResource, size_t aTimestep)
            : fId(aId), fResource(aResource), fTimestep(aTimestep) {}

        /// Required for some std::map things.
        bool operator<(const Job& aRhs) const {
            return fId < aRhs.fId;
        }

        friend std::ostream& operator<<(std::ostream& os, const Job& aJob) {
            os << std::to_string(aJob.fId) << ":" << std::to_string(aJob.fResource)
               << ":" << std::to_string(aJob.fTimestep);
            return os;
        }
    };

    class NodeJobAllocation {
    public:
        using const_iterator = std::map<Resource, Job>::const_iterator;

        NodeJobAllocation() = default;
        void allocate(const Resource& aResource, const Job& aJob) {
            fJobScheduledOnResource.emplace(aResource, aJob);
        }

        const_iterator begin() const { return fJobScheduledOnResource.begin(); }
        const_iterator end() const { return fJobScheduledOnResource.end(); }

        /// Stringification for testing.
        friend std::ostream& operator<<(std::ostream& os, const NodeJobAllocation& aAllocs) {
            for (auto alloc : aAllocs.fJobScheduledOnResource) {
                os << "r#" << std::to_string(alloc.first.fId)
                   << " -> j#" << std::to_string(alloc.second.fId) << " || ";
            }
            return os;
        }

        /// Increase one time step, return freed resource(s).
        std::set<Resource> step() {
            std::set<Resource> freedResources;
            for (auto& jobSched : fJobScheduledOnResource) {
                if (!--jobSched.second.fTimestep) {
                    freedResources.emplace(jobSched.first);
                }
            }
            return freedResources;
        }
    private:
        std::map<Resource, Job> fJobScheduledOnResource;
    };

    class Sched {
    public:
        /// Constructor does the full work of allocating resources.
        Sched(const std::string&& aNodeToResource, const std::string&& aResourceToTimestep);

        /// Get a nice string representation
        friend std::ostream& operator<<(std::ostream& os, const Sched& aSched) {
            for (auto alloc : aSched.fNodeJobAllocationAtTimestep) {
                os << alloc.first << " >> " << alloc.second << std::endl;
            }
            return os;
        }
    private:
        Resource resourceJustLargeEnoughToFit(size_t aRequired);
        NodeJobAllocation allocateOneTimestep();

        PairVector fNodeToResource;
        PairVector fResourceToTimestep;

        /// Ideally, these would be std::unordered_set, but there is no time to write hashing
        /// functions.
        std::set<Resource> fResources;
        std::set<Job> fIncomingJobs;
        std::set<Job> fJobQueue;

        /// A timestamp and a nodeJobAllocation.
        std::vector<std::pair<size_t, NodeJobAllocation>> fNodeJobAllocationAtTimestep;
    };
}
